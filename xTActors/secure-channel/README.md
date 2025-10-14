
On initial Socket Interface creation, we set up a socket state object, and generate unencrypted reader/writer Actors.
These Actors are wired into the Async Socket protocol, for that socket, provided by Lispworks, and the async comms are
started. Each Socket Interface can act as both client / server.

Wire transmissions are performed on self-sync octet streams produced by marshal encoding whatever arguments are being
sent.

Servers silently listen for incoming connection requests from clients.  The only service initially offered is
+SERVER-CONNECT-ID+ (a universally known UUID), which performs an X3DH negotiation with clients to establish a secure
encrypted connection. A separately keyed encrypted channel is set up for each client connection attempt.

We distinguish Channels and Sockets - Channels are logical pipelines feeding a Socket. The Socket represents a physical
interface. There could be multiple Channels on each Socket. Initially, there is only an unencrypted Channel on each
Socket. Once a successful X3DH connection is negotiated then a secure encryption pipeline is attached to the unencrypted
Channel, forming a secure Channel.

Clients initiate communications by choosing a fresh UUID to represent them at the server, a
random 256-bit secret integer and the corresponding random ECC point expressed in Elligator encoding.

The client then sends the server this random ECC Point plus a short packet of information encrypted to the hash of the server's
public key multiplied by the secret integer. The server can derive the decryption key by multiplying the random ECC point
by their secret key. The info packet contains the service desired, currently +SERVER-CONNECT-ID+, 
the UUID chosen to represent the client over the socket connection, and the client's public key ID.

_[A public key ID is not the same as the public key. It is a UUID that refers to an encrypted database entry containing the actual
public key. A database is held by each network node, containing entries for all known participants and their public key
and ID.

In order to join a group like this and be recognized, you need to be introduced by a member who sends your ID and PKey
over an ecrypted channel to each participating node. The member vouches for you.]_

When a server is first contacted by a client, the server sets up an ephemeral service with UUID +SERVER-CONNECT-ID+, which is
widely known to the public as a handshake service to establish an encrypted channel.

Servers validate the client parameters - the random and public key ECC points must be valid curve points, the client ECC public
key must be among those authorized for access. Once validated the server responds, over the unencrypted channel, back to that
client connection ID. Any invalid parameters elicits total silence from the server, and the server immediately closes the socket connection.

The server chooses a random 256-bit integer and its corresponding ECC point expressed in Elligator 
encoding. The server sends that random point back to the client, along with an encrypted information packet. The packet
contains the client's chosen UUID, and a fresh UUID to represent an encrypted channel back to the server. The encryption
key for this packet is the random integer multiplied by the client's public key. The client derives the decryption key by
multiplying the random Elligator point by their secret key.

The server then prepares an encrypted channel with keying derived from X3DH using its secret key, the client's public key, 
the random integer chosen by the server, and the random ECC point shared by the client.

On reply to the client, the client performs similar validation checks against the supplied server random ECC point, and
server public ECC key. Then it sets up its side of the encrypted channel using an X3DH key based on its own secret key, 
the server's public key, the random integer chosen by the client, and the random ECC point shared by the server.

After that, all subsequent communications occur across an encrypted channel, on both sides of the connection, 
using a double ratchet evolving keying beginning with the initial keying derived from X3DH on both sides. The keying changes
with each sent packet.

If anything fails during X3DH negotiation at the server, no response is generated back to the client. Similarly, if the
server response is invalid at the client end, the connection is silently dropped.

Connection ID's are either setup up universally, as with +SERVER-CONNECT-ID+, or as freshly generated UUIDs that
represent an Actor. These ID's may either be permanent, for the duration of the connection, or ephemeral for one-time use
as reply-to addresses.

Actor arguments from each side are translated by the channels into Actor Proxy UUIDs for transmission to the other side
and serve as channel ID's. On the receiving end, these UUIDs are recognized as Actor proxies, and so local proxy Actors
are set up to represent them locally on the machine. These local proxy Actors receive messages and transmit them over the
secure channel to the Actor on the other side identified by the proxy ID.

Almost anything can be transmitted, by deep copy, across a network connection, except for compiled functions and
closures, or objects which contain such.

Actors are an exception to this rule, with the invention of freshly generated UUID's to stand in for them. Objects
containing references to Actors have their references replaced by UUID proxy addresses before transmission to the other
side. Forwarding ephemeral proxy Actors are installed in the channel for each conversion. This is all handled by the
channel, during object marshaling, and should appear transparent to the user, no matter how deeply buried these Actor
references may be.

Marshaling of data converts arbitrary Lisp objects and collections into serializable octet vectors. Unmarshaling performs
the opposite conversion back into Lisp objects.

Data sent between socket nodes is marshaled, compressed, encrypted, authenticated, and then encoded in self-sync framing 
for transmission across the network. We do not provide signatures on the encrypted data packets so that we can have refutable 
encryption. Immediately after receiving a valid encrypted packet, the nodes broadcast the authentication keying derived for the 
incoming packet. In that way, it becomes possible for anyone to forge a transcript of encrypted communications between any two nodes.
There is, therefore, no way to prove that either of the nodes actually held that conversation. The communications are refutable. 
Yet, each participant is assured that the packets were produced by their partner, since only they have the correct keying 
produced by the double-ratchet.

Sockets are free to slice self-sync encoded chunks into smaller packets, but TCP/IP, as a stream protocol, ensures
correct order of receipt among these packets. But since reassembly relies on Actors and message passing, each arriving
packet is numbered sequentially so that the self-sync reader can process them in sequence. Self-sync encoding discerns
chunk boundaries among partial chunk packets.

Connections are protected against oversized packets, invalid packet contents, and packet replay attacks.
Messages are reassembled from received chunks, once all chunks have been received, and may be flagged as invalid
if they contain unexpected contents, or if they fail to decompress and unmarshal properly.

Failures are handled silently and result in a failure to communicate.  No notification is issued, so the only reliable
way to handle cross platform communications involves timeouts and retries. These must be managed by the user code, if
needed.

Socket connections remain open for a limited duration (20s). So long as they are being used their timeout is extended. If
they remain idle for longer than the timeout period, the connection is closed and the channel, and all waiting proxy
Actors, are discarded. But as soon as another communication is attempted, a new socket connection, including a new X3DH
exchange, will be automatically established.



