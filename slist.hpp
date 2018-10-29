#include <memory>
#include <utility>
#include <optional>
#include <functional>

namespace Felix 
{
  template<typename T>
    struct slist_node {
      T head;
      std::shared_ptr<slist_node<T> > tail;
      slist_node (T h, std::shared_ptr<slist_node<T> > t) : head(h), tail(t) {}
    };

  // slist is a wrapper for the shared pointer
  // we use it to provide methods enforcing invariants
  template<typename T>
    class slist {
      using slist_rep = std::shared_ptr<slist_node<T> >;

      slist_rep data;

      // cons
      slist (T head, slist_rep tail) : data(
        std::make_shared<slist_node<T> > (slist_node<T> (head, tail))
      ) {}

      // copy constructor
      slist (slist_rep const & other) : data (other) {}

      // move constructor
      slist (slist_rep && other) : data (other) {}

      // cons type constructor
      template<typename U>
        friend slist<U> cons(U,slist<U>);

    public:
      // type constructor: Empty
      slist () : data (nullptr) {}


      // check if empty
      bool empty() const { return data.get() == nullptr; }

      // destructor: head; undefined on empty
      T head () const { return data.head; }

      // destructor: tail; undefined on empty
      T tail () const { return slist (data.tail); }
    
      // destructor: tail; undefined on empty
      std::pair<T,slist> uncons () const { return make_pair (head(),tail()); }  

      std::optional<std::pair<T,slist> > decode() const {
        if (empty()) return std::nullopt;
        return std::optional<std::pair<T,slist> > (uncons());
     } 
     
     template<typename R>
       R match(
        std::function<R()> Fempty, 
        std::function<R(T,slist)> Fcons) 
       const 
       {
         if (empty()) return Fempty();
         return Fcons (head(), tail());
       }
  };

  // type constructor: Cons
  template<typename T>
    inline slist<T> cons (T h, slist<T> t) { return slist (h,t.data); }

} // namespace

