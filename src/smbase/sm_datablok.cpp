// datablok.cc            see license.txt for copyright and terms of use
// code for datablok.h
// Scott McPeak, 1998-2000  This file is public domain.

#include "sm_datablok.h"
#include "sm_exc.h"
#include "sm_crc.h"
#include "sm_syserr.h"

#include <stdio.h>        // printf
#include <cstring>       // std::memcpy
#include <ctype.h>        // isprint


// define the endpost byte as something we hope is
// unlikely to coincidentally be written during an
// overrun
byte const DataBlock::endpost = 0xBB;


void DataBlock::init(int allocatedSize)
{
  xassert(allocatedSize >= 0);
  dataLen = 0;
  allocated = allocatedSize;
  if (allocated) {
    data = allocate(allocated);
  }
  else {
    data = NULL;
  }

  SELFCHECK();
}


STATICDEF byte *DataBlock::allocate(int size)
{
  byte *ret = new byte[size+1];
  ret[size] = endpost;
  return ret;
}


void DataBlock::selfCheck() const
{
  if (!( 0 <= dataLen && dataLen <= allocated )) {
    breaker();    // having trouble discovering the precise state under gdb
  }
  xassert(0 <= dataLen && dataLen <= allocated);
  xassert( (data==NULL) == (allocated==0) );
  xassert( data==NULL || data[allocated]==endpost );
}


DataBlock::DataBlock(int allocatedSize)
{
  init(allocatedSize);
  SELFCHECK();
}


DataBlock::DataBlock(char const *srcString)
{
  init(0);
  setFromString(srcString);
  SELFCHECK();
}


void DataBlock::ctor(byte const *srcData, int dataLen)
{
  init(0);
  setFromBlock(srcData, dataLen);
  SELFCHECK();
}


void DataBlock::ctor(byte const *srcData, int srcDataLen, int allocatedSize)
{
  init(allocatedSize);
  dataLen = srcDataLen;
  std::memcpy(data, srcData, dataLen);
  SELFCHECK();
}


DataBlock::DataBlock(DataBlock const &obj)
{
  init(obj.allocated);
  copyCtorShared(obj);
}

void DataBlock::copyCtorShared(DataBlock const &obj)
{
  dataLen = obj.dataLen;
  if (dataLen > 0) {
    std::memcpy(data, obj.data, dataLen);
  }
  SELFCHECK();
}


DataBlock::DataBlock(DataBlock const &obj, int minToAllocate)
{
  init(max(obj.getAllocated(), minToAllocate));
  copyCtorShared(obj);
}


DataBlock::~DataBlock()
{
  try {
    SELFCHECK();
    if (data) {
      delete[] data;
    }
  }
  CAUTIOUS_RELAY
}


bool DataBlock::allEqual(DataBlock const &obj) const
{
  SELFCHECK();
  return allocated == obj.allocated &&
         dataEqual(obj);
}


bool DataBlock::dataEqual(DataBlock const &obj) const
{
  SELFCHECK();
  if (dataLen != obj.dataLen ||
      (dataLen > 0 &&
       0 != std::memcmp(data, obj.data, dataLen))) {
    return false;
  }
  else {
    return true;
  }
}



void DataBlock::setDataLen(int newLen)
{
  SELFCHECK();
  xassert(0 <= newLen && newLen <= allocated);
  dataLen = newLen;
  SELFCHECK();
}


void DataBlock::setAllocated(int newAllocated)
{
  SELFCHECK();
  xassert(newAllocated >= 0);
  if (allocated != newAllocated) {
    // allocate new buffer
    byte *newData = NULL;
    if (newAllocated > 0) {
      newData = allocate(newAllocated);
    }

    // truncate defined data
    if (dataLen > newAllocated) {
      dataLen = newAllocated;
    }

    // transfer data
    if (dataLen > 0) {
      std::memcpy(newData, data, dataLen);
    }

    // deallocate old buffer and replace with new buffer
    delete[] data;
    data = newData;
    allocated = newAllocated;
  }
  SELFCHECK();
}


void DataBlock::ensureAtLeast(int minAllocated)
{
  if (allocated < minAllocated) {
    setAllocated(minAllocated);
  }
}


void DataBlock::growDataLen(int changeAmount)
{
  ensureAtLeast(getDataLen() + changeAmount);
  changeDataLen(changeAmount);
}


void DataBlock::addNull()
{
  SELFCHECK();
  data[dataLen] = 0;
  setDataLen(dataLen + 1);
  SELFCHECK();
}


void DataBlock::setFromString(char const *srcString)
{
  SELFCHECK();
  int len = std::strlen(srcString)+1;
    // a sm_string is its contents and the null terminator
  setFromBlock((byte const*)srcString, len);
  SELFCHECK();
}

void DataBlock::setFromBlock(byte const *srcData, int len)
{
  SELFCHECK();
  if (len > allocated) {
    setAllocated(len);
  }
  setDataLen(len);
  if (len > 0) {
    std::memcpy(data, srcData, len);
  }
  SELFCHECK();
}


DataBlock& DataBlock::operator= (DataBlock const &obj)
{
  SELFCHECK();
  if (this != &obj) {
    setAllocated(obj.allocated);
    dataLen = obj.dataLen;
    std::memcpy(data, obj.data, dataLen);
  }
  SELFCHECK();
  return *this;
}


void DataBlock::print(char const *label, int bytesPerLine) const
{
  xassert(bytesPerLine >= 1);
  SELFCHECK();

  if (label) {
    printf("---- %s, length = %d, crc32 = 0x%lX ---- {\n",
           label, getDataLen(),
           crc32(getDataC(), getDataLen()));
  }

  int cursor = 0;
  while (cursor < getDataLen()) {
    int linelen = min(bytesPerLine, getDataLen() - cursor);
    xassert(linelen >= 1);    // ensure can't loop infinitely

    printf("  ");     // indent
    printHexLine(getDataC() + cursor, linelen, bytesPerLine);
    printf("   ");
    printPrintableLine(getDataC() + cursor, linelen);
    printf("\n");

    cursor += linelen;
  }

  if (label) {
    printf("}\n");
  }
  SELFCHECK();
}


// print 'length' bytes of 'data' in hex
// blank-pad the output as if 'linelen' bytes were present
STATICDEF void DataBlock::printHexLine(byte const *data, int length, int linelen)
{
  xassert(data != NULL &&
          length >= 1 &&
          linelen >= length);

  for (int i=0; i<linelen; i++) {
    if (i < length) {
      printf("%02X ", (byte)*data);
      data++;
    }
    else {
      printf("   ");
    }
  }
}


// print 'length' bytes of 'data', substituting 'unprintable' for bytes for
// which 'isprint' is false
STATICDEF void DataBlock::printPrintableLine(byte const *data, int length,
                                             char unprintable)
{
  xassert(data != NULL &&
          length >= 1);

  while (length--) {
    if (isprint(*data)) {
      printf("%c", *data);
    }
    else {
      printf("%c", unprintable);
    }
    data++;
  }
}


#if 0
void DataBlock::print(char const *label) const
{
  enum { MARGIN = 70 };

  if (label) {
    printf("------ %s (length=%d) -------\n", label, getDataLen());
  }

  byte *p = data;
  int i;
  int column=0;
  for (i=0; i<dataLen; i++, p++) {
    if (isprint(*p)) {
      if (*p != '\\') {
        column += printf("%c", *p);
      }
      else {
        printf("\\\\");     // otherwise '\\','x','nn','nn' would be ambiguous
      }
    }
    else {
      column += printf("\\x%02X", *p);
    }

    if (column >= MARGIN && (i+1) < dataLen) {
      printf("\\\n");       // continuation lines end with backslash
      column = 0;
    }
  }

  // this makes spaces at the end of a buffer invisible.. oh well..
  if (column != 0) {    // if didn't just newline...
    printf("\n");
  }

  if (label) {
    printf("------ end of %s -------\n", label);
  }
}
#endif // 0


void DataBlock::dontPrint(char const *, int) const
{}


void DataBlock::writeToFile(char const *fname) const
{
  FILE *fp = fopen(fname, "wb");
  if (!fp) {
    xsyserror("fopen", fname);
  }

  // finally giving in and silencing those *stupid* g++ warnings
  // about comparing signed and unsigned for EQUALITY!!!
  // I'll get you yet, you big stinking GNU!!
  if ((int)fwrite(getDataC(), 1, getDataLen(), fp) != getDataLen()) {
    xsyserror("fwrite", fname);
  }

  if (fclose(fp) != 0) {
    xsyserror("fclose", fname);
  }
}


void DataBlock::readFromFile(char const *fname)
{
  FILE *fp = fopen(fname, "rb");
  if (!fp) {
    xsyserror("fopen", fname);
  }

  // seek to end to know how much to allocate
  if (fseek(fp, 0, SEEK_END) != 0) {
    xsyserror("fseek", fname);
  }

  long len = ftell(fp);
  if (len < 0) {
    xsyserror("ftell", fname);
  }

  setAllocated(len);

  // read data
  if (fseek(fp, 0, SEEK_SET) != 0) {
    xsyserror("fseek", fname);
  }

  if ((long)fread(getData(), 1, len, fp) != len) {
    xsyserror("fread", fname);
  }

  setDataLen(len);

  if (fclose(fp) != 0) {
    xsyserror("fclose", fname);
  }
}


// ------------- self test code --------------
#ifdef DATABLOK_TEST

int doit()
{
  // nest everything so the dtors are inside
  {
    // test printing function
    {
      DataBlock b(260);
      for (int i=0; i<260; i++) {
        b.getData()[i] = (byte)i;
      }
      b.setDataLen(260);
      b.print("all bytes plus 4 extra");
    }

    DataBlock block("yadda smacker");
    xassert(block.getDataLen() == 14);

    DataBlock block2((byte*)"yadda smacker", 13, 14);
    block2.addNull();
    xassert(block == block2);

    DataBlock block3;
    block3 = block2;
    xassert(block3 == block);

    block3.setAllocated(5);       // truncates
    block2.setAllocated(25);
    xassert(block3 != block2);

    // test file save/load
    block.writeToFile("tempfile.blk");
    DataBlock block4;
    block4.readFromFile("tempfile.blk");
    xassert(block == block4);

    // test overrun detection
    try {
      {
        DataBlock b(block);
        b.getData()[block.getAllocated()] = 0;   // overrun

        printf("this should cause an assertion failure:\n");
        // invoke selfcheck in destructor
      }
      return printf("failed to detect overrun\n");
    }
    catch (...) {}
  }

  printf("test succeeded\n");
  return 0;
}

int main()
{
  try {
    return doit();
  }
  catch (xBase &x) {
    return printf("failed: %s\n", x.why());
  }
}

#endif // DATABLOK_TEST

