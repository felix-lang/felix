import fbuild.config.c as c

# ------------------------------------------------------------------------------

class ssl_h(c.Test):
    header = c.header_test('openssl/ssl.h')

class crypto_h(c.Test):
    header = c.header_test('openssl/crypto.h')

class blowfish_h(c.Test):
    header = c.header_test('openssl/blowfish.h')

class des_h(c.Test):
    header = c.header_test('openssl/des.h')

class rc4_h(c.Test):
    header = c.header_test('openssl/rc4.h')

class dsa_h(c.Test):
    header = c.header_test('openssl/dsa.h')

class dh_h(c.Test):
    header = c.header_test('openssl/dh.h')

class engine_h(c.Test):
    header = c.header_test('openssl/engine.h')

class rsa_h(c.Test):
    header = c.header_test('openssl/rsa.h')

class x509_h(c.Test):
    header = c.header_test('openssl/x509.h')

class hmac_h(c.Test):
    header = c.header_test('openssl/hmac.h')

class md2_h(c.Test):
    header = c.header_test('openssl/md2.h')

class md3_h(c.Test):
    header = c.header_test('openssl/md3.h')

class md4_h(c.Test):
    header = c.header_test('openssl/md4.h')

class md5_h(c.Test):
    header = c.header_test('openssl/md5.h')

class mdc2_h(c.Test):
    header = c.header_test('openssl/mdc2.h')

class ripemd_h(c.Test):
    header = c.header_test('openssl/ripemd.h')

class sha_h(c.Test):
    header = c.header_test('openssl/sha.h')

class err_h(c.Test):
    header = c.header_test('openssl/err.h')

class threads_h(c.Test):
    header = c.header_test('openssl/threads.h')

class rand_h(c.Test):
    header = c.header_test('openssl/rand.h')

class opensslv_h(c.Test):
    header = c.header_test('openssl/opensslv.h')

class bio_h(c.Test):
    header = c.header_test('openssl/bio.h')

class evp_h(c.Test):
    header = c.header_test('openssl/evp.h')

class pem_h(c.Test):
    header = c.header_test('openssl/pem.h')

class bn_h(c.Test):
    header = c.header_test('openssl/bn.h')

class buffer_h(c.Test):
    header = c.header_test('openssl/buffer.h')

class lhash_h(c.Test):
    header = c.header_test('openssl/lhash.h')
