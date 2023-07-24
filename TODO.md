# TODO

 - Make Element.bytes return ArraySeq[Byte] rather 
   than Seq[Byte], use ArraySeq.unsafeWrapArray(...)
   on the fully private intermediate array
 - After publishing, be sure to update the library
   version in `README.md` and `examples/Setup.scala` 