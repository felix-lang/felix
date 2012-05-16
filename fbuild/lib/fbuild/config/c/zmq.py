import fbuild.config.c as c

# ------------------------------------------------------------------------------

class zmq_h(c.Test):
    header = c.header_test('zmq.h')
    if header:
      print ("ZMQ .. SUPPORTED")
    else:
      print ("ZMQ .. NOT SUPPORTED")
