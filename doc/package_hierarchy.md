# Layout of the DNSCatcher Packages

DNSCatcher is split into multiple packages that subdivide its functionality into useful subprograms and modules. It is intended that programs intending to link to DNSCatcher will use the public packages listed in this document, and the public APIs defined in the specification files. Interfaces to C and other programming languages are built around the same general hierarchy.

Unless noted, all files are stored under the `src/` top level directory, with package names stored as directories. Standard GNAT Ada file handling is to be used.

For example, `DNSCatcher.Example.Module` would be stored under `src/dnscatcher/example` and have the filename of `dnscatcher-example-module.ad{s,b}`. Functions implemented in C or other programming languages should follow idiomatic practices, or a descriptive name for languages like C.

## DNSCatcher (top level project)

The top-level project for most of the functionality is `DNSCatcher`. The top level API provides simple front-ends to most of the basic functionality, although in certain cases, it may be necessary to instance submodules directly.

Not all of these hierarchies exist as of time of writing, this is intended as a general design document.

This top level project is linked to `libdnscatcher.so.X` or `dnscatcherX.dll` depending on the platform.

### DNSCatcher.API

External interfaces used by the management protocol or a web frontend to access statistical data, reports, domain queries, etc.

Access is gated through user authentication (which is distinct from cross-check users, although some overlap is there).

### DNSCatcher.C_Interface

Implements interfaces to the C programming language which in turn can be wrapped to other languages.

### DNSCatcher.Cache

Handles caching results and records.

### DNSCatcher.Config

Top level package defining the Configuration record, and other components relating to the core libraries configuration and other aspects. Parsers for the config files are located in Util and not here as library users may wish to define their own interfaces separate from the one's used by DNSCatcher.

### DNSCatcher.Config.Crosscheck_Servers
DNS servers to be used for crosscheck by the server.

### DNSCatcher.Config.DNSSEC_Root_Trust_Anchors

Contains interfaces relating to root server KSK management. This is part of the Config hierarchy and not the DNS as the root KSKs can change over time, or in testing or non-ICANN roots, be different than the typically accepted norms. This configuration record can also be automatically updated by the RFC5011 update mechanism.

A mechanism for bootstrapping KSKs is also available for certain test and non-standard configurations.

### DNSCatcher.Config.DLV

Handles management of Domain Lookaside Validation. DLV information can be updated dynamically as needed.

### DNSCatcher.Config.Root_Servers

A static list of DNS root servers and necessary DNS glue code for locating the root. This is loaded from the `db.root` file from modern BIND implementations, but can be reconfigured as necessary.

### DNSCatcher.Config.Forwarding_Servers
What servers should be forced to for recursive resolve if not directly doing it within Catcher.

### DNSCatcher.Cryptography

Contains helper and stub functions relating to implementing DNS-over-TLS, and implementing and validating DNSSEC. It is expected this module will mostly be implemented in C linking to OpenSSL

### DNSCatcher.Cryptography.CFSSL

In certain cases, Catcher may operate as a certificte authority. This functionality is intended to be handled by CFSSL, a standalone Go application for handling TLS certificates. This interface allows interaction with the CFSSL server dynamically.

### DNSCatcher.Cryptography.Key_Management
Handles various keys used by catcher such as local KSK, ZSKs, and conversion of keys to different formats. At a minimium Catcher is expected to dynamically generate it's own KSK/ZSK, and may generate child zones with various signature algorithms for validation of DNSSEC information.

### DNSCatcher.Cryptography.Hash

Collection of hashing functions, likely implemented from OpenSSL. This is used for generating RRSIG information.

### DNSCatcher.Cryptography.Signature_Management

Handles signature creation and validation.

### DNSCatcher.Cryptography.SMIME

Wrapper for creating and managing S/MIME objects.

### DNSCatcher.Cryptography.X509

A separate submodule for handling X509 certificates is required for DNS-over-TLS/DNS-over-HTTPS. This subfunction wraps the following functionality from OpenSSL.

 * Certificate Signing Request generation
 * PEM certificate parsing
 * CApath checking
 * Revocation information checks
   * CRL download
   * OCSP Request
 * OCSP signing (when possible)

### DNSCatcher.Crosscheck

Implements the DNS Crosscheck protocol as defined in the technical overview document in the same folder as this file.

The following submodules are expected.

 * `DNSCatcher.Crosscheck.Users`
   * Handles information from anonymous, registered, and validated users
 * `DNSCatcher.Crosscheck.Validate`
   * Handles building validating paths for a given result.
 * `DNSCatcher.Crosscheck.Discrepancy`
   * Code handling data mismatches
 * `DNSCatcher.Crosscheck.Parse`
   * Parses cross-check information on the sending system
 * `DNSCatcher.Crosscheck.S2S`
   * Handles implementation of the S2S protocol

### DNSCatcher.Database

Backend routines for instancing with various data stores. Support for PostgreSQL is planned, with likely partial support for Firebird, and/or SQLite for the client component of DNSCatcher.

For databases that can handle it, stored procedures are to be used whenever and wherever possible to remove or reduce risk of SQL injection attacks, and to limited which stored procedures a given database user needs to access.

### DNSCatcher.Datasets

Implements specific types of datasets and queues for managing data processing stages, implemented as protected objects, and instanced throughout the code. These objects handle information and any necessary dynamic memory allocation/deallocation.

### DNSCatcher.DNS

Functionality relating to DNS processing and parsing live under this package hierarchy. Commonly used enumerations and variables such as error codes, RRTypes, and more live here.

### DNSCatcher.DNS.Client

A DNS Client interface, built on top of the Catcher core libraries.

The following subpackages are expected.
* `DNSCatcher.DNS.Client.Root_KSK_Updater`
  * Downloads any updates to the root server KSK via RFC 5011, and allows them to be saved.
* `DNSCatcher.DNS.Client.Recursive_Resolve`
  * Implements a recursive resolver from the root zone to the desired record, optionally performing QNAME minimalization. This interface is abstracted on the main client API.
*  `DNSCatcher.DNS.Client.Sigchase`
   *  Implementing "sigchase" functionality where the recursive resolver downloads and checks RRSIG records from the root to the leaf record. This functionality is equivelent to the `dig +sigchase` option, and chains to the KSK and root servers held in the config.
*  `DNSCatcher.DNS.Client.TSIG`
   *  Implements TSIG support for DNS UPDATE operations

The client will support UDP, TCP/IP, DoT, and DoH. It's expected to implement DoH, libcurl will be wrapped and used.

### DNSCatcher.DNS.Server

Implements basic DNS server functionality. The expected mode of operation for DNSCatcher is to act as an end point for test results and other misc data, but it can also function as a recursive resolver (normally done in the client library), caching resolver, or authoritive server.

### DNSCatcher.DNS.Server.Authoritative_Zones

Handles implementation and management for zones that are to be considered authoritative. As the DNS Validator libs may require dynamically updated zones, this interface allows for the creation of these zones on the fly and later deletion.

### DNSCatcher.DNS.Transaction_Manager

Handles functionality relating to managing DNS transaction state. Each interface implementing DNS protocol has it's own instance of the Transaction_Manager.

### DNSCatcher.DNS.Processor

Implements a parser and processor the DNS protocol. Submodules currently planned and implemented as follows. The top level interface takes raw 'over the wire' DNS packets, and converts them to high level records. Inverse functionality is also handled by this module.

The following submodules are planned.
  * `DNSCatcher.DNS.Processor.Packet`
    * Parses raw DNS packets for hand-off to other submodules.
  * `DNSCather.DNS.Processor.RData.*`
    * Handles managing record data information, via a common API and abstraction layer defined in `DNSCather.DNS.Processor.RData`
  * `DNSCatcher.DNS.Processor.RData.OPT`
    * OPT is the special record type that handles EDNS information and validation. As the OPT record requires special handling and influences large parts of how DNS packets are sent and managed, this module has special handling and interfaces in addition to the standard RData ones.

### DNSCatcher.DNS.IDN

Handling for internationalized domain names A<->U record conversion. This is expected to be a wrapper around IDN libraries and not re-implemented from scratch. Submodules for IDN 2003, and 2008 are expected.

### DNSCatcher.Network

Interfaces for sending UDP/TCP information, as well as support (eventually) for TLS and HTTPS communications live here. Each network interface defines a common API to be used by other parts of the codebase, and have their own instance of transaction managers.

The following interfaces are planned to be implemented here. It is intended for all interfaces to support IPv6, however, as of writing, `GNAT.Sockets` does not support IPv6. As such, IPv6 support will be implemented as a wrapped under `DNSCatcher.Network.Implementation.IPv6`. GNAT.Socket support will be wrapped as `DNSCatcher.Network.Implementation.IPv4`. Some protocols may not directly create sockets, for example, `DNS_Over_TLS` may be implemented directly on the OpenSSL BIO interfaces and implemented in C with an Ada wrapper.

 * `DNSCatcher.Network.UDP`
 * `DNSCatcher.Network.TCP`
 * `DNSCatcher.Network.DNS_Over_TLS`
 * `DNSCatcher.Network.DNS_Over_HTTPS`

For creating local control interfaces, or injection of DNS traffic from third-party apps such as Tor, the following interfaces will also be implemented. As these interfaces are not directly tied to DNS wire protocol like the above, during creation, their mode can be specified to DNS_UDP, DNS_TCP, or the unique DCATCHER_CONTROL which acts as an administrative interface.

 * `DNSCatcher.Network.Unix_Domain_Socket`
   * Implements a domain socket interface, as commonly found on UNIX systems. This function will not be implemented for Windows.
 * `DNSCatcher.Network.Named_Pipe`
   * Named pipe interface, which is far more typical on Windows based systems.

In certain cases, DNSCatcher is better run as a passive tap, with no responses being sent out. That functionality is handled via `DNSCatcher.Network.PCap`, and wraps `libpcap`.

### DNSCatcher.Utils

Top level package for utility functionality that doesn't fit anywhere else.

#### DNSCatcher.Utils.Logger

Implements logging API functionality for DNSCatcher. This module is instanced as part of the global config and can be replaced with a different logger or a null implementation as desired. The logger is designed to be thread aware and works on the basis of individual message queues.

The expected logging implementations are as follows

 * `DNSCatcher.Utils.Logger.Stdio`
   * Logs to standard output for interactive usage
 * `DNSCatcher.Utils.Logger.Log_File`
   * Logs to a log file
 * `DNSCatcher.Utils.Logger.Syslog`
   * Logs to syslog (UNIX-systems only)
 * `DNSCatcher.Utils.Logger.Null`
   * No output. Intended for use with the test suite.

### DNSCatcher.Tasks

Tasks that handle processing of various DNSCatcher components; these normally map 1:1 to system threads when compiled with GNAT. The following tasks are excepted to be implemented.

 * `DNSCatcher.Tasks.Network_IO`
   * Handles all network I/O for the project, and dispatches incoming and outgoing data from other components. One task per network interface protocol is expected to ease intergration with TLS libraries.
 * `DNSCatcher.Tasks.Data_Processor`
   * Parses inbound data and generates the necessary responses. `Data_Processor` tasks act as working threads and can be spawned as necessary working from a single data queue collection.
 * `DNSCatcher.Tasks.Logger`
   * Handles the thread aware logger.
 * `DNSCatcher.Tasks.Scheduler`
   * Handles scheduling events such as queue flushes, data deletion, and other functions as essentially an internal crond

### DNSCatcher.Server_Management

Manages aspects of the DNSCatcher server such as dynamic key updates, setting/updating the local zone KSK, and other things that can be managed through the API.

### DNSCatcher.Users

Handles implementation and authentication of users and their access privileges for DNSCatcher. This is distinct from cross-check users.

### DNSCatcher.Test_Manager

Handles managing tests from client Catcher implementations. Test_Manager takes a list of tests the client wishes to run, creates a season identifer for it, and returns a set of endpoints to query. The client then submits its test answers, and the server tallys up the result.

### DNSCatcher.Version

Includes versioning information on the DNSCatcher implementation. Version information is dynamically generated at build time, and includes information relating to build options, build environment, host operating system, and similar criteria. This is used for reporting information, and providing debugging information.

### DNSCatcher.Work_Unit

Code interfaces for generating, distributing, and managing work unit requests.

### DNSCatcher.Zone

Represents information about a DNS zone, as well as functions and subprograms to create and manage an authoritive zone to be served up by the server. Zone data can be collected via DNS, inbound AXFR, loaded from flat file, or from the backend database.

#### DNSCatcher.Zone.DNSSEC

DNSSEC information is not directly encoded into a zone. Instead it is dynamically generated (or validated) via KSK chaining. To that end, this hierarchy contains validation routines and code to "compile" RRSIGs for all the data within a zone file.

## DNSCatcher_Client (top level project)

Implements client side functionality for testing resolutation and other DNS properties when installed on a client system. Like the core DNSCatcher code, this is implemented as a library to allow easy integration into various tools.

This library will be installed as `libdnscather_client.so.X` or `dnscatcher_client.dll`. The client library can be dynamically linked or statically linked with the functionality needed from the core library.

The following modules from DNSCatcher are expected to be used.

 * `DNSCatcher.Config`
 * `DNSCatcher.Cryptography`
 * `DNSCatcher.DNS`
 * `DNSCatcher.DNS.Client`
 * `DNSCatcher.DNS.Prcessor`
 * `DNSCatcher.DNS.Crosscheck`
 * `DNSCatcher.IDN`
 * `DNSCatcher.Network`
 * `DNSCatcher.Tasks.Network_IO`
 * `DNSCatcher.Tasks.Logger`
 * `DNSCatcher.Tasks.Scheduler`
 * `DNSCatcher.Utils.Logger`

It is also expected an external library for making HTTPS REST requests will be included, likely in the form of a wrapper around libcurl.

### DNSCatcher_Client.C_Interfaces

Implements a C interface to the client library

### DNSCatcher_Client.Crosscheck

Client side interface for the DNS cross-check interface.

### DNSCatcher_Client.DNS_Tests.*

Location of all test code through the backend. Configuration information is loaded by the test manager once the server/client negotiate a test set.

### DNSCatcher_Client.HTTP_DNS_Query

To deal with limited support in web browsers for raw DNS requests, a local interface for making arbitrary DNS requests is available through 127.0.0.1.

### DNSCatcher_Client.Test_Manager

Handles the client side aspects of determining which tests can be run, and collecting information, as well as getting the set of tests the client support, and the tests the server supports.

### DNSCatcher_Client.Version

Versioning information for the client, sent to the Catcher Server.

### DNSCatcher_Client.Work_Unit

Handles processing and managing WUs.

## DNSCatcher_Daemon (top level project)

Implements the freestanding DNSCatcher daemon which is installed to `bin/dnscatcherd`

## DNSCatcher_Client_Daemon (top level project)

Implements the freestanding client daemon. Compiled to `bin/dnscatchercd`.

## Test_DNSCatcher.*

Unit tests and intergration tests for all subprograms. This is stored in the top level tests/ directory, and linked to a static binary known as test harnass, which can read in a config file if necessary to setup it's environment.