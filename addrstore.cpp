#include <stdio.h>
#include <memory.h>
#include <stdint.h>
#include <stdlib.h>
#include <map>
#include <chrono>
#include <assert.h>
#include "Judy.h"

namespace Addrstore
{
typedef void *address;
typedef uintptr_t word;

// 13 = 5 + 8

//------------------------------------------------------
// 8 bit lookup with realloc
//

uint64_t nNode8=0;
uint64_t nNode5=0;
uint64_t nNode16=0;
uint64_t nNode32=0;

// We use a trick, allocating only powers of 2 store
int alloc_amt(uint8_t  used_amt) {
  if (used_amt == 0) return 0;
  int i = 1;
  while(i < used_amt) i *= 2;
//printf("Current used %d -> alloc amt %d\n", used_amt,i);
  return i;
}

struct Node8 {
  uint8_t *keys;
  address *data;
  int used;
  Node8() : used(0), keys(NULL), data(NULL) { ++nNode8; }
  ~Node8() { free(keys); free(data); }
  address find(uint8_t key) {
    for (int i=0; i < used; ++i)
      if (keys[i]==key) return data[i];
    return NULL;
  }

  void insert(uint8_t key, address datum) {
    for (int i=0; i < used; ++i)
      if (keys[i]==key) { data[i]=datum; return; }
    int n = 0;
    if (used == alloc_amt(used)) {
      // change from full to half full before inserting
      n = alloc_amt (used+1);
//printf("Realloc in Node8, current used=%d amt_used calc = %d new used amt= %d\n",used, alloc_amt(used), n);
      keys = (uint8_t*)realloc(keys, n);
      data = (address*)realloc(data , n  * sizeof (address));
    }
    keys[used]=key;
    data[used]=datum;
    ++used;
//printf("Insert8 key %p data %p used %d, calc alloc %d, actual alloc %d\n",address(word(key)),datum, used, alloc_amt(used),n);
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
  Node5() { memset((void*)map,0,32 * sizeof(address)); ++nNode5; }
  address find (uint16_t key) { assert(key<32); return map[key]; }
  void insert(uint32_t key, address data) { 
//printf("Node5 insert key = %d\n", key);
     assert(key < 32);
     map[key] = data; 
//printf("Insert5 key %p data %p\n",address(word(key)),data);
  }
  void remove(uint32_t key) { map[key] = NULL; }
};

//------------------------------------------------------
// 16 bit lookup only
// 
struct Node16 
{
  Node16() {++nNode16; }
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
//printf("Node 16 insert, STL map starts\n");
    lowtab.insert (::std::pair<uint32_t, void*>(key,data));
//printf("Insert16 key %p data %p\n",address(word(key)),data);
  }
  void remove (uint32_t key) {
    lowtab.erase(key);
  }

};

//------------------------------------------------------
// rewrite to keep the store sorted
// needed for find < etc
// 32 bit lookup only
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
  Linear32() : store(NULL), n_used(0), n_max(0) {++nNode32;}

  // will work if store is sorted, may be faster
  // than binary chop if n_used is small
  address find (uint32_t key) {
    if(!store) return NULL;
    for(size_t i = 0; i<n_used; ++i)
      if(key == store[i].key) return store[i].data;  
    return NULL;
  }

  void insert(uint32_t key, address data) { // unchecked
    int i = 0;
    while (i<n_used && store[i].key < key)++i;
    if(i<n_used && store[i].key==key)  // overwrite key
      store[i].data = data;
    else {
      if (n_used == n_max) { // need more space
        n_max = n_max?2*n_max : 1;
        store = (Node32kv*) realloc (store, n_max * sizeof (Node32kv));
      }
      if(i!=n_used) { // not at end
        memmove(store+i+1,store+i,sizeof(Node32kv) * (n_used-i));
      }
      store[i].key = key;
      store[i].data = data;
      ++n_used;
    }
//printf("Insert32 key %p data %p\n",address(word(key)),data);
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
    
//printf("Find key=%p\n",key);
    uint32_t r31_0 = (uint32_t)word(key);           // 32 bits residual
    uint32_t k63_32 = (uint32_t)(word(key) >> 32);  // 32 bit key
//assert((word(k63_32) << 32) + word(r31_0)==word(key));

    uint16_t r15_0 = (uint16_t)r31_0;               // 16 bits residual
    uint16_t k31_16 = (uint16_t)(r31_0 >> 16);      // 16 bit key
//assert((word(k31_16) << 16) + word(r15_0)==r31_0);

    uint16_t r10_0 = r15_0 & 0x7FF;                  //  11 bits residual
    uint8_t k15_11 = (uint8_t)(r15_0 >> 11);        //   5 bit key
//assert((word(k15_11) << 11) + word(r10_0)==r15_0);


    uint8_t k10_3 = (uint8_t)(r10_0>> 3);             //  8 bits
//assert((word(k10_3) << 3) ==r10_0);
    // low 3 bits 2_0 must be 0                     //  3 bits unused
   
    //printf("k63_32=%16llx\nk31_16=%16llx\nk15_11=%16llx\nk10_3 =%16llx\n",word(k63_32),word(k31_16),word(k15_11),word(k10_3));
//    assert((word(k63_32) << 32) + (word(k31_16) << 16) + (word(k15_11) << 11) + (word(k10_3) << 3) == word(key));

    void *result = top.find(k63_32);                       // 32 bit lookup
//printf("Find63_32 key=%p -> %p\n",address(word(k63_32)),result);
    if (result) result = ((Node16*)result)->find(k31_16);   // 16 bit lookup
//printf("Find31_16 key=%p -> %p\n",address(word(k31_16)),result);
    if (result) result = ((Node5*)result)->find(k15_11);    // 5 bit lookup
//printf("Find15_11 key=%p -> %p\n",address(word(k15_11)),result);
    if (result) result = ((Node8*)result)->find(k10_3);     // 8 bit lookup
//printf("Find10_3 key=%p -> %p\n",address(word(k10_3)),result);
    return result;
  }

  void insert(address key, address data)
  {
    uint32_t r31_0 = (uint32_t)word(key);           // 32 bits residual
    uint32_t k63_32 = (uint32_t)(word(key) >> 32);    // 32 bit key

    uint16_t r15_0 = (uint16_t)r31_0;               // 16 bits residual
    uint16_t k31_16 = (uint16_t)(r31_0 >> 16);        // 16 bit key

    uint16_t r10_0 = r15_0 & 0x7FF;                  //  11 bits residual
    uint8_t k15_11 = (uint8_t)(r15_0 >> 11);               //   5 bit key


    uint8_t k10_3 = (uint8_t)(r10_0>> 3);             //  8 bits
    // low 3 bits 2_0 must be 0                     //  3 bits unused
//printf("Insert starting\n");

    Node16 *result16 = (Node16*)(top.find(k63_32));  // 32 bit lookup
    if(!result16) {
//printf("Can't find Node16, creating\n");
      Node8 *nu8 = new Node8;
      nu8->insert (k10_3,data);
      Node5 *nu5 = new Node5;
      nu5->insert (k15_11,nu8);
      Node16 *nu16 = new Node16;
      nu16->insert (k31_16,nu5);
      top.insert (k63_32,nu16);
      return;
    }

    Node5 *result5 = (Node5*)(result16->find(k31_16));    // 16 bit lookup 
    if(!result5) {
//printf("Can't find Node5, creating\n");
      Node8 *nu8 = new Node8;
//printf("Node 8 created\n");
      nu8->insert (k10_3,data);
//printf("k10_3 inserted into Node 8 created\n");
      result5 = new Node5;
//printf("Node 5 created\n");
      result5->insert (k15_11,nu8);
//printf("k15_11 inserted into Node 5\n");
      result16->insert(k31_16,result5);
//printf("k31_16 inserted into Node 16\n");
      return;
    }

    Node8 *result8 = (Node8*)(result5->find(k15_11));     // 5 bit lookup
    if(!result8) {
//printf("Can't find Node8, creating\n");
      result8 = new Node8;
      result8->insert (k10_3,data);
      result5->insert (k15_11,result8);
      return;
    }

//printf("Inserting into Node8\n");                      // 8 bit insert
    result8->insert (k10_3,data);
    return;
  }

};

} // end namespace Addrstore

struct JudyLArrayWrapper
{
  typedef Addrstore::address address;
  void *ja;
  JError_t je;
  JudyLArrayWrapper () : ja(NULL) {}

  void insert (address key, address data) {
    address *p = (address*)JudyLIns(&ja,(Word_t)key,&je);
    *p = data;
  }

  address find (address key) {
    address *slot = (address*)JudyLGet(ja,(Word_t)key,&je);
    if (slot) return *slot;
    return NULL;
  }
};

struct STLMapWrapper
{
  typedef Addrstore::address address;
  std::map<address,address> mp;

  void insert(address key, address data) {
    mp.insert (::std::pair<address, address>(key,data));
  }

  address find (address key) {
    auto it = mp.find(key);
    if (it == mp.end()) return NULL;
    return (*it).second;
  }
};

using namespace Addrstore;

// hacked random number generator, 64 bits
address randw () {
  union U {
    uint8_t bytes[8];
    word w;
  };
  U u;
  for(int i = 0; i<8; ++i) u.bytes[i]=rand();
  return address(u.w & ~0x3 & 0x000000FFFFFFFFFF); // mask low 3 bits and hi 24 bits off
}


void check(int sample, address *save)
{

  // Insert data
  Map64 m;

  auto start =std::chrono::system_clock::now(); 
  for (int i=0; i<sample; ++i) {
   //printf("%3d -> %.16p\n",i,save[i]); // no compliant but works on OSX
   auto k = m.find (save[i]);
   if (k != NULL)
   {
     printf("Duplicate key index %d, key=%p, found data=%ld\n", i, save[i], word(k));
     save[i]=0;
   }

   m.insert (save[i],address(word(i)));
  }
  auto end =std::chrono::system_clock::now(); 
  auto elapsed = std::chrono::duration_cast<std::chrono::microseconds> (end - start).count();
  printf("Inserts done, elapsed = %7.3f s, rate = %lld per ms\n",elapsed / 100000.0, sample * 1000 / elapsed);

  // Vertify data
  start =std::chrono::system_clock::now(); 
  for (int i=0; i<sample; ++i){
   //printf("verify %3d -> %.16p\n",i,save[i]); // no compliant but works on OSX
   //printf(" Result %d should be %d\n", (int)(word)m.find(save[i]), (int)word(i));
   if (save[i]!=0) {
     assert (m.find (save[i]) == address(word(i)));
   }
  }
  end =std::chrono::system_clock::now(); 
  elapsed = std::chrono::duration_cast<std::chrono::microseconds> (end - start).count();
  printf("Verify done, elapsed = %7.3f s, rate=%lld per ms\n",elapsed / 100000.0,sample * 1000 / elapsed);

  printf("REPEATING WITH JUDYL\n");
  JudyLArrayWrapper ja;

  start =std::chrono::system_clock::now(); 
  for (int i=0; i<sample; ++i) {
   //printf("%3d -> %.16p\n",i,save[i]); // no compliant but works on OSX
   auto k = ja.find (save[i]);
   if (k != NULL)
   {
     printf("Duplicate key index %d, key=%p\n", i, k);
     save[i]=0;
   }

   ja.insert (save[i],address(word(i)));
  }
  end =std::chrono::system_clock::now(); 
  elapsed = std::chrono::duration_cast<std::chrono::microseconds> (end - start).count();
  printf("Judy Insert done, elapsed = %7.3f s, rate=%lld per ms\n",elapsed / 100000.0,sample * 1000 / elapsed);

  start =std::chrono::system_clock::now(); 
  for (int i=0; i<sample; ++i){
   //printf("verify %3d -> %.16p\n",i,save[i]); // no compliant but works on OSX
   //printf(" Result %d should be %d\n", (int)(word)m.find(save[i]), (int)word(i));
   if (save[i]!=0) {
     assert (ja.find (save[i]) == address(word(i)));
   }
  }
  end =std::chrono::system_clock::now(); 
  elapsed = std::chrono::duration_cast<std::chrono::microseconds> (end - start).count();
  printf("Judy Verify done, elapsed = %7.3f s, rate=%lld per ms\n",elapsed / 100000.0,sample * 1000 / elapsed);


  printf("REPEATING WITH STL MAP\n");
  STLMapWrapper mp;

  start =std::chrono::system_clock::now(); 
  for (int i=0; i<sample; ++i) {
   //printf("%3d -> %.16p\n",i,save[i]); // no compliant but works on OSX
   auto k = mp.find (save[i]);
   if (k != NULL)
   {
     printf("Duplicate key index %d, key=%p\n", i, k);
     save[i]=0;
   }
   mp.insert (save[i],address(word(i)));
  }
  end =std::chrono::system_clock::now(); 
  elapsed = std::chrono::duration_cast<std::chrono::microseconds> (end - start).count();
  printf("STL Map Insert done, elapsed = %7.3f s, rate=%lld per ms\n",elapsed / 100000.0,sample * 1000 / elapsed);

  start =std::chrono::system_clock::now(); 
  for (int i=0; i<sample; ++i){
   //printf("verify %3d -> %.16p\n",i,save[i]); // no compliant but works on OSX
   //printf(" Result %d should be %d\n", (int)(word)m.find(save[i]), (int)word(i));
   if (save[i]!=0) {
     assert (mp.find (save[i]) == address(word(i)));
   }
  }
  end =std::chrono::system_clock::now(); 
  elapsed = std::chrono::duration_cast<std::chrono::microseconds> (end - start).count();
  printf("STL Map Verify done, elapsed = %7.3f s, rate=%lld per ms\n",elapsed / 100000.0,sample * 1000 / elapsed);
}

int main() {
  int sample = 1000000;
  printf("Hello addrstore sample size=%d\n", sample);
  address *save = (address*)malloc(sample * sizeof(address));

  printf("RANDOM ALLOCATION\n");
  // create data
  auto start =std::chrono::system_clock::now(); 
  for (int i=0; i<sample; ++i)
  {
    address r = randw();
    //printf("%3d -> %.16p\n",i,r); // no compliant but works on OSX
    save[i] = r;
  }
  auto end =std::chrono::system_clock::now(); 
  auto elapsed = std::chrono::duration_cast<std::chrono::microseconds> (end - start).count();
  printf("Samples done, elapsed = %7.3f s\n",elapsed / 100000.0);

  check(sample, save);

  printf("SEQUENTIAL ALLOCATION\n");
  for (int i=0; i<sample; ++i)
   save[i]= address (word (((i * 8) /8) * 8));

  check(sample, save);

  printf("Node population count\nNode32=%lld\nNode16=%lld\nNode5=%lld\nNode8=%lld\n",
   nNode32, nNode16,nNode5,nNode8);
}
