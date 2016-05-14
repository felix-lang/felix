#include <stdio.h>
#include <memory.h>
#include <stdint.h>
#include <stdlib.h>
#include <map>

typedef void *address;

// 13 = 5 + 8

//------------------------------------------------------
// 8 bit lookup with realloc
//

// We use a trick, allocating only powers of 2 store
int alloc_amt(unsigned char used_amt) {
  for(int i=0; used_amt!=0; used_amt>>=1,i*=2)
    return i;
}

struct Node8 {
  unsigned char *keys;
  address *data;
  int used;
  Node8() : used(1) { keys = malloc(1); data = malloc(sizeof(address)*1) {} 

  void find(uint8_t key) {
    for (int i=0; i < used; ++i)
      if (keys[i]==key) return data[i];
    return NULL;
  }

  void insert(uint8_t key, address *datum) {
    for (int i=0; i < used; ++i)
      if (keys[i]==key) { data[i]=datum; return; }
    if (used = alloc_amt(used)) {
      keys = realloc(keys, 2 * used);
      data = realloc(data , 2 * used * sizeof (address));
    }
    keys[used]=key;
    data[used]=datum;
    ++used;
  }



};


//------------------------------------------------------
// 5 bit lookup
// full fanout

struct Node5 {
  address map[32];
  Node5() { memset((void*)map,32 * sizeof(void*), 0); }
  address find (uint16_t key) { return map[key]; }
  void insert(uint32_t key, address data) { map[key] = data; }
  void remove(uint32_t key) { map[key] = NULL; }
};

//------------------------------------------------------
// 32 bit lookup only
// 
struct Node16 
{
  Node16() {}
  ::std::map<uint32_t, void*> lowtab;

  // find pointer to structure containing 
  // table for low 16 bits
  // We use a C++ map for this
  address find(uint16_t key) {
    auto result = lowtab.find (key);
    if (result == lowtab.end()) return NULL;
    return (*result).second;
  }
  void insert(uint32_t key, address data) {
    lowtab.insert (::std::pair<uint32_t, void*>(key,data));
  }
  void remove (uint32_t key) {
    lowtab.erase(key);
  }

};

//------------------------------------------------------
// rewrite to keep the store sorted
// needed for find < etc
// 64 bit lookup only
class Linear32 
{
  struct Node32kv
  {
     uint32_t key;
     address data;
  };

  size_t n_used;
  size_t n_max;
  Node32kv *store;
public:
  Linear32() : store(NULL), n_used(0), n_max(0) {}

  // will work if store is sorted, may be faster
  // than binary chop if n_used is small
  address find (uint32_t key) {
    if(!store) return NULL;
    for(size_t i = 0; i<n_used; ++i)
      if(key == store[i].key) return store[i].data;  
    return NULL;
  }

  void insert(uint32_t key, address data) { // unchecked
    if (n_used == n_max) {
      n_used *= 2;
      store = (Node32kv*) realloc (store, n_max * sizeof (Node32kv));
    }
    store[n_used].key = key;
    store[n_used++].data = data;
  } 

  // will work if store is sorted too
  address remove(uint32_t key) { // unchecked
    for(size_t i = 0; i<n_used; ++i)
      if(key == store[i].key) {
        address data = store[i].data;
        if (i != n_used - 1)
          memcpy(
            store + i * sizeof(Node32kv), 
            store + (i + 1) * sizeof(Node32kv),
            (n_used - i - 1) * sizeof(Node32kv)
          );
        --n_used;
        return data;
      }
      return NULL;
  }

};

int main() {
 printf("Hello addrstore\n");
}
