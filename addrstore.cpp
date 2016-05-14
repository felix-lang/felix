#include <stdio.h>
#include <memory.h>
#include <stdint.h>
#include <stdlib.h>
#include <map>

namespace Addrstore
{
typedef void *address;
typedef uintptr_t word;

// 13 = 5 + 8

//------------------------------------------------------
// 8 bit lookup with realloc
//

// We use a trick, allocating only powers of 2 store
int alloc_amt(uint8_t  used_amt) {
  int i = 0;
  for(;used_amt!=0; used_amt>>=1,i*=2);
  return i;
}

struct Node8 {
  uint8_t *keys;
  address *data;
  int used;
  Node8() : used(0), keys(NULL), data(NULL) {}

  address find(uint8_t key) {
    for (int i=0; i < used; ++i)
      if (keys[i]==key) return data[i];
    return NULL;
  }

  void insert(uint8_t key, address *datum) {
    for (int i=0; i < used; ++i)
      if (keys[i]==key) { data[i]=datum; return; }
    if (used == alloc_amt(used)) {
      // change from full to half full before inserting
      keys = (uint8_t*)realloc(keys, 2 * used);
      data = (address*)realloc(data , 2 * used * sizeof (address));
    }
    keys[used]=key;
    data[used]=datum;
    ++used;
  }

  void remove (uint8_t key) {
    for (int i=0; i < used; ++i)
      if (keys[i]==key) {
        if (i!=used) {
          memcpy(keys+i, keys+i+1,used-i-1);
          memcpy(data+i, data+i+1,(used-i-1) * sizeof(address));
        }
        --used;
        if (used == alloc_amt(used)) {
          // change from half full to full after inserting
          keys = (uint8_t*)realloc(keys, used);
          data = (address*)realloc(data , used * sizeof (address));
        }
      } 
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

//------------------------------------------------------
// full address lookup
class Map64
{
  Linear32 top;

public:
  address find(address key)
  {
    
    uint32_t r31_0 = (uint32_t)word(key);           // 32 bits residual
    uint32_t k63_32 = (uint32_t)(word(key) >> 32);    // 32 bit key

    uint16_t r15_0 = (uint16_t)r31_0;               // 16 bits residual
    uint16_t k31_16 = (uint16_t)(r31_0 >> 16);        // 16 bit key

    uint8_t r10_0 = r15_0 & 0x7FF;                  //  11 bits residual
    uint8_t k15_11 = word(key) >> 11;               //   5 bit key


    uint8_t k10_3 = (uint8_t)r10_0>> 3;             //  8 bits
    // low 3 bits 2_0 must be 0                     //  3 bits unused
    
    void *result = top.find(k63_32);                       // 32 bit lookup
    if (result) result = ((Node16*)result)->find(k31_16);   // 16 bit lookup
    if (result) result = ((Node5*)result)->find(k15_11);    // 5 bit lookup
    if (result) result = ((Node8*)result)->find(k10_3);     // 8 bit lookup
    return result;
  }
};

} // end namespace Addrstore

int main() {
 printf("Hello addrstore\n");
}
