# Revision history for h3spec

## 0.1.14

* Using `http3` v0.1.7 and `quic` v0.3.13.
* 27 new test cases, 77 in all; none of the old ones are removed.
* QUIC: CRYPTO_BUFFER_EXCEEDED for CRYPTO data buffered beyond the limit;
  FRAME_ENCODING_ERROR for an ACK range below zero; PROTOCOL_VIOLATION
  for an ACK of a packet never sent, for STREAM_DATA_BLOCKED and
  DATA_BLOCKED in Handshake packets, and for a RETIRE_CONNECTION_ID never
  issued; TRANSPORT_PARAMETER_ERROR for a malformed parameter value and
  for initial_max_streams_bidi or initial_max_streams_uni over 2^60.
* HTTP/3 and QPACK: H3_STREAM_CREATION_ERROR for a second control,
  encoder or decoder stream and for a client's push stream;
  H3_CLOSED_CRITICAL_STREAM for a closed encoder or decoder stream and
  for an encoder stream ending inside an instruction;
  QPACK_DECOMPRESSION_FAILED for a reference to a dynamic table entry
  that is not there; a request with a connection-specific field treated
  as malformed, one case for each of connection, keep-alive,
  proxy-connection, transfer-encoding, upgrade and TE other than
  trailers, and TE with trailers accepted; a frame longer than
  SETTINGS_MAX_FIELD_SECTION_SIZE not buffered; H3_FRAME_ERROR for a
  SETTINGS frame ending mid-parameter; settings read past a reserved
  identifier, and a repeated identifier treated as an error.
* The cases that open a second control, encoder or decoder stream, or a
  push stream, are pending against a server that lets a client have only
  the three unidirectional streams it needs, since such a stream cannot
  be opened there at all.
