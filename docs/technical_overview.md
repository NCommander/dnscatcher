# Technical Overview of the DNSCatcher Project

This document is a high level design and overview of the DNSCatcher system, it's underlying mechanisms, data collection mechanisms, and security measures put in place. It is intended as an introduction to the project, as well as an overview of how it intends to provide research into the DNS recursive resolver infrastructure, as well as provide a deeper level of sanity checking and security to recursive resolvers.

## DNS Ecosystem As Of 2019

DNS is one of the core protocols of the Internet, primarily used to map IP addresses such as 172.217.12.142 to google.com. DNS is designed as a highly distributed hierarchy system that descends from the root server zone to individual domains distributed throughout the Internet. Heavy caching allows it to handle millions requests per second through a series of caching and recursive resolvers that serve as end points to the DNS.

However, as a protocol designed in the late 80s however, DNS was not built with security in mind. Several efforts, most notable DNSSEC and more recently, DNS-over-TLS/DNS-over-HTTPS has been brought forward to try and help improve the underlying integrity of the DNS system. Unfortunately, neither of these mechanisms provide true end to end security to DNS. Two mechanisms have emerged to help provide a deeper level of security to the ecosystem, but do not solve a fundamental issue within DNS - specifically recursive resolvers can lie.

### The Problem: Lying Recursive Resolvers

As previously described, DNS is designed as a distributed system; end user systems typically only talk to a DNS resolver that is given to them by DHCP during network configuration. Unless manually overridden, an end system will typically only that resolver for all lookups.

DNSSEC (described below) information is not relayed to the end-user unless specifically requested (and validated) and most DNS client implementations depend on the the recursive resolver to perform all validation and lookups. As DNSSEC information is not typically checked at the last mile, nor is universally available, this leaves the recursive resolver free to manipulate the data as it sees fit.

For example, my home ISP redirects non-existent webpages (ones that would return NXDOMAIN) to a parking site with advertising. DNS hijacking is also used for implementing captive portals such as wifi hotspot signin pages. In other cases, DNS record manipulation can be used for censorship, man-in-the-middle attacks, and other malicious uses.

Even in non-malicious cases, recursive resolvers can either lie or be buggy. As an example of a non-malicious use case PiHole (https://pi-hole.net/) acts as a DNS blacklist to filter out advertising ing web sites. If the security measures currently implemented in DNS were fully effected; it would be impossible for either of these types of non-malicious hijackings to occur. In other cases, buggy or misconfigured resolvers prevent resolution of record types beyond the standard A and CNAME types.

To further complicate matters, as typical DNS traffic (on port 53) is not encrypted or signed, a malicious actor can re-write records in flight. This type of attack is known and can be used to execute cache poisoning attacks (https://en.wikipedia.org/wiki/DNS_spoofing#Cache_poisoning_attacks) on properly configured and behaving DNS resolvers.

While DNS-over-TLS/DNS-over-HTTPS specifically protect against these, deployment of these technologies beyond the web browser has been at best limited. Let us take a moment to step by and look at the two major security enhancements to DNS and show that they do not entirely solve the problems listed above.

### DNSSEC 

DNSSEC in and of itself is designed to provide a level of authentication to the DNS system by providing a system of signing records returned by the system. In DNSSEC, the root zone is signed with a special KSK that is embedded in DNS clients and servers that serve as a trust anchor for the entire ecosystem. Top level domains then can be signed, and then in turn second level and higher domains signed through a series of "designed signer" (DS) records.

DNSSEC suffers from some of the inherent issues of the distributed nature of DNS. Because there is no "central" server in DNS, lookups for individual domains and their DNSSEC records can take multiple roundtrips to walk the chain. DNSSEC secured records are signed with a special DNS record type known as RRSIG, which is a signed hash of records of a given type. Validating a given RRSIG is a process known as walking the chain and can require multiple DNS requests to multiple servers on the case of cold caches.

For example, to validate my personal domain of casadevall.pro, a recursive resolver must first query the root zone for the .pro DS record, query the pro TLD for it's DNSKEY (the public key), and the DS records from casadevall.pro, and then query casadevall.pro for it's DNSKEY, and then get the individual records requested. Only then with all DS and DNSKEY records can the RRSIG be validated and chained up to the root.

These records are normally validated by the recursive resolver (which is usually operated by an ISP or on a router on users premise) and cached. Clients querying their local resolver can check the status of DNSSEC information by checking the AD bit of the DNS options flags. Importantly, **unless a client specifically requests the RRSIG records and walks the chain itself a recursive resolver can freely manipulate returned records**.

Clients that wish to check DNSSEC information for a given host set EDNS option DO, which indicates that a client can accept DNSSEC records. Upstream resolvers are then supposed to return RRSIGs for a given record if they exist. If DO=1 is set, but a zone is not signed, then the recursive resolver simply returns the requested record or NXDOMAIN. Validation that a zone is *not* signed is somewhat more complex. Take the following examples:

My personal domain, casadevall.pro is signed, and thus can be validated from the root to the leaf node, and the RRSIGs are sent if DO=1 (which is set with dig with the +dnssec option:

```
$ dig casadevall.pro +dnssec

;; flags: qr rd ra; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 1

casadevall.pro.		3600	IN	A	96.126.124.51
casadevall.pro.		3600	IN	RRSIG	A 8 2 3600 20190722113545 20190622111815 57243 casadevall.pro. GIT/ZVNQbskL6FUquUH1oVasfU585OJYyPSxcYzdfnPtzH092cRX1drI BHrJGYrf0vGbJ0kYtsooyo7b1gsZtp7wgqMt4PzimLW5J2/R2kl02UPf TSj6jVXL8CpHPOk/vIbQT7DecwABet8MnSbnG1hIlKiw3bbTFR/VkJFm 0NoHdbJHxh3pfAL+V1B7cl1qWwOVLRq4o8OX2CFQuc00Z0tyWSzUEatE k3BVIuYLJafBLkAhcrzPIoUsSNUiIoG+beJXqn5qR+Q0ehV6/Qc/qQZ3 vhe8dm9vudu19elA+f36pe4P9CdN7DujhtJAunrCKuu3I1VE/3Nuya/K OuSsew==
```

Notable, the recursive resolver in my setup **did not** set the AD-bit, showing that although I could retrieve DNSSEC information, it was not validated by the recursive resolver (nor did it return SERVFAIL as it should if the chain failed to validate)

As I know that this site is correctly signed, to validate this RRSIG set, I need to walk the entire chain. dig provides a command to do this known as sigchase. This command in theory validates the chain end to end. However, when this command is run against the NetGear WNR1000v2 at my mom's apartment, sigchase fails:

```
$ dig @10.0.0.1 +sigchase +trusted-key=./root.keys casadevall.pro
;; RRset to chase:
casadevall.pro.		3600	IN	A	96.126.124.51

[..]
Launch a query to find a RRset of type DNSKEY for zone: casadevall.pro.

[..]

Launch a query to find a RRset of type DS for zone: casadevall.pro.

[..]

;; VERIFYING DNSKEY RRset for casadevall.pro. with DNSKEY:57243: success
;; OK this DNSKEY (validated by the DS) validates the RRset of the DNSKEYs, thus the DNSKEY validates the RRset
;; Now, we want to validate the DS :  recursive call

Launch a query to find a RRset of type DNSKEY for zone: pro.
;; NO ANSWERS: no more

;; DNSKEY is missing to continue validation: FAILED
```

It is unclear why validation fails, although it is clearly a problem with the recursive resolver on the Netgear router. Validation succeeds if I use the Google Public DNS server:

```
$ dig @8.8.8.8 +sigchase +trusted-key=./root.keys casadevall.pro
;; RRset to chase:
casadevall.pro.		3599	IN	A	96.126.124.51

[..]

;; WE HAVE MATERIAL, WE NOW DO VALIDATION
;; VERIFYING DS RRset for pro. with DNSKEY:59944: success
;; OK We found DNSKEY (or more) to validate the RRset
;; Ok, find a Trusted Key in the DNSKEY RRset: 59944
;; Ok, find a Trusted Key in the DNSKEY RRset: 25266
;; Ok, find a Trusted Key in the DNSKEY RRset: 20326
;; VERIFYING DNSKEY RRset for . with DNSKEY:20326: success

;; Ok this DNSKEY is a Trusted Key, DNSSEC validation is ok: SUCCESS
```

This is a clear example of how DNSSEC to the last mile has both real world problems in deployment, and how a recursive resolver can lie; it is impossible for this NetGear router to return AD=1 for casadevall.pro as it can not properly validate the chain. It is possible that Spectrum (the ISP in question in this scenario) is resolving it, and the router is echoing the AD=1 bit, but it is impossible for a client system to be able to know for sure.

Furthermore, proving that a site is not signed is complicated as it can't be done in a single query. At the time of writing. cnn.com is not a signed zone. Making a DNS request to 8.8.8.8 with DO=1 returns the following.

```
$ dig @8.8.8.8 cnn.com +dnssec
;; ANSWER SECTION:
cnn.com.		42	IN	A	151.101.65.67
cnn.com.		42	IN	A	151.101.129.67
cnn.com.		42	IN	A	151.101.1.67
cnn.com.		42	IN	A	151.101.193.67
```

To validate the zone is not signed, I need to query for the DS records.

```
$ dig @8.8.8.8 cnn.com DS +dnssec
;; AUTHORITY SECTION:
CK0POJMG874LJREF7EFN8430QVIT8BSM.com. 86399 IN NSEC3 1 1 0 - CK0Q1GIN43N1ARRC9OSM6QPQR81H5M9A  NS SOA RRSIG DNSKEY NSEC3PARAM
CK0POJMG874LJREF7EFN8430QVIT8BSM.com. 86399 IN RRSIG NSEC3 8 2 86400 20190710225036 20190703214036 3800 com. amNReyUb+VLjAaq5wwy08nOZvLI6ay9LbfA0xs2jQX7tRobX3axhWMeB pyEJ+kyXw1HeSTWIyrTT1fsA62Pc0FYvSNJ9WSNAC8qeOrYZeI1bd+TX /PzLlFLq2Jjhki7Z5dH/1Xi5JDQh8uZiHZ2YgoNxfitrCyk8TKcDVrTl QvQ=
com.			899	IN	SOA	a.gtld-servers.net. nstld.verisign-grs.com. 1562444047 1800 900 604800 86400
com.			899	IN	RRSIG	SOA 8 1 900 20190713201407 20190706190407 3800 com. gog7Hn1K9A0fUmTS5UYh1B7rWIyjyhRPE2SguYrrR8f8UDD9Rn7qis3z +CQPPKAU9BWhc14SZkVSwEmaUtmyOOvmy7H9uIHGavV8zZ8f4KkjVM7Q wYSQxJn+IaoAHdLGodVZWlCsOMWrZYqA5cZoZKWVK3eJXej7dNCoUerQ ykk=
FVT71LMDJ71M5N4BBJG7S42QT4H2K0VS.com. 86399 IN NSEC3 1 1 0 - FVT8070RVMMN14H33TU31073GPDT89UQ  NS DS RRSIG
FVT71LMDJ71M5N4BBJG7S42QT4H2K0VS.com. 86399 IN RRSIG NSEC3 8 2 86400 20190710052140 20190703041140 3800 com. ch1CQ2+5a/A6CCiF9un8Va963z1W62pDA2kF8nMvWBKRIdksYWZMJMPI j0ey9LM7YLDfJh3nXyQXFRb6osSA+8xCof0hjL18V88PU3NWhCefgDlU uVULE6NDhvEYGthRW9+9KGLN722epWXREaQC7wy2LwB6yH0P4krpZIr/ gRw=

```

This query returns an NSEC3 (Next SECure v3) response for the DS record, saying that this record does not exist, and the NSEC3 response is signed with the .org DS keys from the root, validating that the zone is not signed.

In summary, while DNSSEC provides a secure mechanism for validating zone information, DNS resolvers can easily manage this data as seen by the NetGear router causing the sigchase to fail. While this can be bypassed by using "known good" DNS servers, this complicates coding, and introduces issues in corporate environments where split-horizon DNS may be deployed.

Further complicating issues is that DNSSEC has very low deployment. As of July 6th, 2019, StatDNS (https://www.statdns.com/), reports that there are 140,646,272 domains in the .com TLD. Of these, 1,242,177 are signed. That means a deployment ratio of 0.8%, an appalling low number.

In summary, DNSSEC is both difficult to validate, may be impossible to validate without use of specifically "known good" resolvers, and only covers a small percentage of all Internet domains means that in and of itself it does not provide a good mechanism for securing the DNS ecosystem. It is also unclear how many DNS resolvers handle tampering of DNSSEC data (for example, if all DNSSEC information down to the root is stripped), and if they soft-fail, or properly hard fail.

### DNS-over-TLS/DNS-over-HTTPS

DNS-over-TLS (DoT) and DNS-over-HTTPS (DoH) represent the second standardized mechanisms to help protect and secure DNS traffic. As these methods have nearly identical characteristics, I'm only going to cover DNS-over-TLS unless otherwise noted. DoT suffers from problems that will limit it's deployment, and adds a tremendous layer of complexity to a core Internet protocol while also failing to solve some of more fundamental issues with DNS. To understand these problems, we need to understand the trust mechanisms used in TLS, how they integrated with DNS, issues in deployment, and other related topics.

As the name suggests, DoT simply wraps traditional DNS calls over the Transport Layer Security protocol. TLS is the industry standard used to secure internet traffic, and provides both authentication and encryption functionality; for example HTTPS is simply the standard HTTP protocol wrapped around TLS which provides enough security to allow transmission of credit card information. It is for this reason that DoH provides and inhertits the same security and risk factors that DoT does. TLS is underpinned by a system of certificate authorities that act as trust anchors in a manner similar to the root KSK in DNSSEC. The trust mechanism used on the Internet is collectively known as Web Public Key Infrastructure or WebPKI for short.

WebPKI is an extremely complicated best, but I will try to simplify it as best as possible. In WebPKI, both web browsers, and operating systems provide a set of "known good" CA root certificates. Google, Mozilla, Microsoft, and Apple all operate trust stores that contain the set of certificates they trust. Many open source projects as well as Linux distributions such as Debian and Ubuntu use the Mozilla certificate store as it is free and open source. As of writing, there are 154 root certificates in this store (https://ccadb-public.secure.force.com/mozilla/IncludedCACertificateReport).

Like the KSK, CA root certificates are essentially hard-coded into operating system and TLS libraries. While it is almost always possible for an end user to add or remove roots, generally deployment or removal of a root certificate can take upwards of several years to deploy as the root store must first be updated and then trickle down via various methods to end users.

CA rules and regulations are managed by the Certificate Authority/Browser (CA/B) Forum which publishes the Baseline Requirements (BR) on how certificates may be issued, auditing requirements, and similar rules and regulation. As of writing, the current version of the Baseline Requirements is 1.6.5 (https://cabforum.org/wp-content/uploads/CA-Browser-Forum-BR-1.6.5.pdf), and will be referenced from this point forward.

Root certificates are extremely powerful, they can sign for any domain on the Internet, and if misused or misissued, could be devastating for security on the public Internet. As they form trust anchors on the Internet, removal of a root certificate is non-trivial as they can not simply be revoked; instead a software update to remove a compromised or non-compliant root must be made.

According to CA/B rules, root certificates can not issue leaf certificates directly. This is a security measure due to the revocation mechanisms (discussed later) built into TLS. Instead, root certificate issue a special type of certificate known as an intermediate certificate which can sign on it's behalf. This allows for the root certificate to be kept offline and airgapped, while an intermediate certificate handles online signing of end user CSRs. In case of compromise, intermediate certificates can (theoretically) be revoked without impacting the root.

As such, the chain of validation looks like the following: Root -> Intermediate -> Leaf certificate. X.509 certificates only allow for a single Issuer so there is only a single path to be followed to the root. This is similar to KSK path valiation. It is also possible for multiple intermediate certificates to be used; this is common in some S/MIME setups where a technically-constrained S/MIME intermediate certificate is issued to an organization.

#### Anatomy of TLS Certificates
To further understanding, let's take a look at the TLS certificates protecting Google's DoT server and how they chain up to the root and are validated. The openssl command provides a useful interface for working with and managing x509 certificates. Let's connect to Google's DoT server and examine the certificates:

```$ openssl s_client -verify 2 -CAfile /etc/ssl/certs/ca-certificates.crt -showcerts -connect 8.8.8.8:853

verify depth is 2
CONNECTED(00000003)
depth=2 OU = GlobalSign Root CA - R2, O = GlobalSign, CN = GlobalSign
verify return:1
depth=1 C = US, O = Google Trust Services, CN = Google Internet Authority G3
verify return:1
depth=0 C = US, ST = California, L = Mountain View, O = Google LLC, CN = dns.google
verify return:1
---
Certificate chain
 0 s:/C=US/ST=California/L=Mountain View/O=Google LLC/CN=dns.google
   i:/C=US/O=Google Trust Services/CN=Google Internet Authority G3
[..]
-----END CERTIFICATE-----
---
Server certificate
subject=/C=US/ST=California/L=Mountain View/O=Google LLC/CN=dns.google
issuer=/C=US/O=Google Trust Services/CN=Google Internet Authority G3
---
No client certificate CA names sent
Peer signing digest: SHA256
Server Temp Key: X25519, 253 bits
---
SSL handshake has read 3106 bytes and written 261 bytes
Verification: OK
---
New, TLSv1.2, Cipher is ECDHE-RSA-CHACHA20-POLY1305
Server public key is 2048 bit
Secure Renegotiation IS supported
Compression: NONE
Expansion: NONE
No ALPN negotiated
SSL-Session:
    Protocol  : TLSv1.2
    Cipher    : ECDHE-RSA-CHACHA20-POLY1305
    Session-ID: 884B11906781449C234AF5B19DD975F3F213FA399E20139D16C9A05A10EB6293
    Session-ID-ctx: 
    Master-Key: D729A4BFBFE048E8D88E9335F10754307D5A790983744E3ECD63317C99DB74DE15EC5700A84BBE829F46BC0E927666E4
[..]
```

The s_client command instructs openssl to start a TLS session, in this case to 8.8.8.8 on port 853 which is the well-known port for DNS-over-TLS. The CAfile option points to Ubuntu's root server store in /etc/ssl/certs/ca-certificates.crt, while -verify forces full validation of the chain. -showcerts prints the certificates to the console; which has been excluded for brevity.

As seen in the above output, we have three certificates in the chain, the root certificate, the intermediate, and leaf, as described above. Let's look at the leaf certificate and examine the pertinent information:


```
$ openssl x509 -in /tmp/google-leaf.pem -noout -text
[..]
    Signature Algorithm: sha256WithRSAEncryption
        Issuer: C = US, O = Google Trust Services, CN = Google Internet Authority G3
        Validity
            Not Before: Jun 18 08:39:54 2019 GMT
            Not After : Sep 10 08:16:00 2019 GMT
        Subject: C = US, ST = California, L = Mountain View, O = Google LLC, CN = dns.google
[..]
        X509v3 extensions:
            X509v3 Extended Key Usage: 
                TLS Web Server Authentication
            X509v3 Subject Alternative Name: 
                DNS:dns.google, DNS:*.dns.google.com, IP Address:2001:4860:4860:0:0:0:0:64, IP Address:2001:4860:4860:0:0:0:0:6464, IP Address:2001:4860:4860:0:0:0:0:8844, IP Address:2001:4860:4860:0:0:0:0:8888, IP Address:8.8.4.4, IP Address:8.8.8.8, DNS:8888.google, DNS:dns.google.com, DNS:dns64.dns.google
            Authority Information Access: 
                CA Issuers - URI:http://pki.goog/gsr2/GTSGIAG3.crt
                OCSP - URI:http://ocsp.pki.goog/GTSGIAG3

            X509v3 Subject Key Identifier: 
                6E:09:F2:6C:CE:59:5C:15:7B:59:8A:E7:6C:AD:26:5F:FE:BF:C9:C3
            X509v3 Basic Constraints: critical
                CA:FALSE

[..]
            X509v3 CRL Distribution Points: 

                Full Name:
                  URI:http://crl.pki.goog/GTSGIAG3.crl

[..]
```

Leaf certificates describe the entity they are protecting, and can be summarized by the subject line:  *C = US, ST = California, L = Mountain View, O = Google LLC, CN = dns.google*

In this example, this certificate protects the resource dns.google (CN or common name), and the certificate authority has validated that the receiving organization is Google. As X.509 certificates can cover multiple domain names, and IP addresses, that information is covered in a section known as the subjectAlternativeName, listed above which covers various domain and IP addresses that Google's Public DNS service runs on.

We can also see that the certificate was issued by Google Trust Services's which is an intermediate certificate for the GlobalSign root certificate (which Google acquired in 2016).

#### TLS Certificate Revocation and AIA

In addition, the certificate contains information on revocation servers known as "Authority Information Access" or AIA. AIA provides mechanisms for checking revocation status of a given certificate. In TLS, there are two methods of revocation, certificate revocation lists (CRL), or online certificate status protocol (OCSP). BR requirements that all certificate authorities support OCSP; CRL support is optional but common. We can use this information to confirm that the certificate is valid

OCSP:
```
$ openssl ocsp -issuer /tmp/google-intermediate.pem -cert /tmp/google-leaf.pem -url http://ocsp.pki.goog/GTSGIAG3 -CAfile /etc/ssl/certs/ca-certificates.crt 
WARNING: no nonce in response
Response verify OK
/tmp/google-leaf.pem: good
	This Update: Jul  6 14:07:41 2019 GMT
	Next Update: Jul 13 14:07:41 2019 GMT
```

CRL:
```
# Download the CRL from the AIA information:
$ wget  http://crl.pki.goog/GTSGIAG3.crl

# OpenSSL can't directly validate against a DER formatted CRL, so convert it to PEM
$ openssl crl -inform DER -in GTSGIAG3.crl -outform PEM -out crl.pem

# Merge everything into one file for openssl verify
$ cat google-intermediate.pem crl.pem > fullchain-crl.pem

# Validate against CRL
$ openssl verify -crl_check -CAfile fullchain-crl.pem google-leaf.pem 
google-leaf.pem: OK
```

As seen in the above, certificate revocation checking is a non-trivial affair. Further complicating matters is that there is an inhabitant bootstrapping problem; revocation information is encoded in X.509 certificates as a domain, as such, a DNS lookup is required to successfully check the revocation status of a DoT server. Furthermore, even if OCSP/CRL information was encoded as an IP address, it still requires communicating with a third-party server to check the certificate which leaks information to the certificate authority.

In response to this problem, there is a mechanism known as OCSP-Stapling which embeds a OCSP signature in the TLS handshake, proving the state of revocation as part of a normal connection. OCSP-Stapling is well supported for HTTPS, but other server products have some to no support for it. Notably, Google's DoT server does not support OCSP Stapling.

The issues relating to TLS certificate revocation are long and complicated, and thus I recommend reading Gibson's Research Corporation's report on certificate revocation (https://www.grc.com/revocation/ocsp-must-staple.htm) for more information. In summary though, revocation is an extremely difficult thing to get correct in WebPKI.

#### Issues Relating to Deployment

DoT/DoH also suffer real world deployment issues, specifically on cases of private networks (RFC1918) space, or IPv6 ULA space. By CA/B forum standards, a TLS certificate can't be issued for these areas as they're not accessible from the public internet. For enterprise deployments, it may be possible to sidestep the problem by use of an internal CA (for example, Microsoft Active Directory offers this functionality out of the box).

However, deployment and management of an internal CA is not trivial, and in some environments, extremely difficult to distribute the root CA certificate to end user clients. Furthermore, deployment in residental, small offices networks, or guest networks that allow bring-your-own-devices will also be unable to properly validate against internally deployed DoT servers.

In many cases, sites operate on the basis of split horizon DNS (this is extremely common with Active Directory) where users within a LAN can see a set of DNS entries, while the external DNS has a different set of entries, or the same addresses with different IPs (such as a mail server). Since one must use the corporate DNS server, these users are entirely unable to take advantage of these new technologies.

#### No Changes to the DNS Wire Protocol

Finally, DoT/DoH do not change the wire protocol of DNS at all; it is simply a wrapper around the 1980s base protocol and it's extensions. As such, a DoT/DoH server remains fully capable of manipulating traffic as shown above with DNSSEC.

As such, while DoT does provide protection against passive wiretapping, and in-flight modification of DNS records, it does not provide any real security as to the integrity of DNS contents. This is something that DNSCatcher intends to address.

## Theory of Operation

DNSCatcher is designed to run "in tandem" with existing DNS implementations, and provide a third-party verification and assertion that said records are correct and cross-checked. While it still suffers from the problem that a malicious DNSCatcher server could lie about verification status and data, it is designed to be accountable through a variety of mechanisms employed to ensure that any malicious servers are easily detected.

To this end, production DNSCatcher servers are required to be deployed with a OV or EV certificate from a well-known CA provider (special handling is available for enterprise and RFC1918 deployment cases). As DV certificates only verify the domain name, and not the organization, they are not suitable for this purpose. During registration (see below) the user is presented with the certificate information to provide accountability; this, combined with strict checking of revocation information provides a strong level of accountability as a Catcher operator must be validated by a certificate authority and their information displayed to an end-user.

Furthermore, DNSCatcher servers must be secured by TLSA/DANE while in production mode, and the ZSK/KSK they use for signing CHK_IN responses over either interface(see below) must either be the same key, or chained to said key. By requiring strict accountability of Catcher servers, any malicious actor can be detected and blacklisted.

Once deployed, the Catcher server itself provides a trust anchor on the authenticity of DNS records, as well as providing a cross-section of DNS recursive resolver behavior for use with research purposes on the health of the DNS ecosystem.

During the course of normal operations, the Catcher client resolves DNS lookups via the standard system configured DNS resolver, and verifies those lookups against the Catcher server; submitting both the DNS question and response to the Catcher server. The Catcher server builds "known good" information from a variety of sources, and relays to the client how a path was built for verification and a score to determine if information is correct (see Server Operations, path building). Responses are cryptographically signed with a secure timestamp embedded in it.

This tandem approach allows for enterprise and ISP provided services that depend on split-horizon DNS to work correctly while providing a level of confidence that other DNS traffic is not being manipulated or hijacked. It also provides a mechanism to use these DNS resolvers and still confirm validity of DNSSEC information and other DNS security markers, side-stepping the issues related to DoT described above.

Currently, it is planned for a Catcher response to contain the following information; this information is a draft for informational purposes and is subject to change:

 * Records seen by the DNSCatcher server in regards to the DNS Question
   * These records are sent in the CHK_IN class and not in the IN class as authoritative answers
 * Information on how DNS information was validated (see below)
 * Full DNSSEC chains
   * For correctly signed domains, the entire chain is sent
   * For malformed signed domains, SERVFAIL is set
     * Additional information is sent as Additional Responses
   * For unsigned domains, the chain up to the point where a NSEC(3) record is encountered for DS, and terminated
   * For DLV, the DLV trust anchor + full chain as above
     * DLV is not expected to be commonly used
 * Information on Catcher's data sources and methods to build a path
 * A status message on verification, one of the following states
   * valid
   * valid-secure
     * This record passed DNSSEC validation to the root KSK
   * valid-stale
     * the local resolver returned old DNS records that haven't reached their TTL and the Catcher server could confirm this)
   * state-unconfirmed
     * An examination of the SOA of a given domain shows that the records are likely valid, but the upstream server doesn't have them cached to confirm this.
   * invalid
     * DNSSEC validation failed when the server performed the lookup, but the client did not receive SERVFAIL
   * split-horizon
     * The Catcher server got NXDOMAIN trying to validate it or got a public record while the client didn't. Split horizon status is determined if the above is true and the following rules are met. The rough plan for detecting split horizon is as follows:
       * The IP address on the client's records pointed to local address space, or within the same allocation block (determine via RIP lookups if possible).
       * The client's normal DNS resolver is within said address space
       * The domain lookup was for a known "local" TLD such as .lan, .local, or .home
       * The DNS search path is part of the domain being looked up
    * blackholed
      * The client got NXDOMAIN, but the Catcher server was able to resolve the address.
    * client-only
      * The server got an error during lookup, but the client successfully managed to get a response; this either indicates a split horizon scenario that falls outside of the rules, or DNS hijack.
    * conflict
      * The Catcher client and server got differing responses, and the server was unable to determine any realistic way the client could have gotten said record set.
      * The Catcher server may issue Work Units to try to better understand the state of a conflict.
    * needs-more
      * The server needs more information to come to a determination
    * unknown
      * The server couldn't come to a determination

Furthermore, the Catcher infrastructure allows for research into the behavior of recursive resolvers. A work unit system is implemented in the protocol that users may opt-in to. This allows the Catcher server to request the client perform look-ups on it's behalf and submit it's results to the central server. This mechanism is designed to primarily test the recursive resolver infrastructure and provide insight to how DNS records are resolved, advance understanding of name collision issues relating to new top-level domains, and understanding hijacking and censorship implemented on the DNS resolver level.

## Server Operations

DNSCatcher servers act as a central repostiory of trust and validation for DNS records. Each server is an independent island, although it is possible for one server to be slaved to another (for example, a DNSCatcher server running on a Tor exit node could be slaved to an upstream server that collects data from all exit nodes). Each server's identity is secured by verified X.509 certificates as described above.

DNSCatcher should be deployed on it's own IPv4 and IPv6 address.

### X.509 Encoded KSK
An important note with the operation of DNSCatcher is that since it is a centralized service providing authentication, it is vulnerable to malicious operators providing false assurances or misleading data to clients. This problem is inhertiant to any type of trust authority and thus Catcher's approach is to ensure that server operators are verified to say whom they are. Identication services offered by CAs are used to attest the ownership of a given private key.

At it's core, both DNSSEC and TLS can make use of RSA2048 key pairs, abiet it with different encodings. This is also true of elliptic-curve cryptography pairs, but as EC-DNSSEC is not well supported as of yet, this feature is deferred for now.

As such the same public and private key pair used for a server's KSK can be re-encoded in the X.509 certificate format, and presented in the form of a Certificate Signing Request. This CSR can then be submitted to any well-trusted CA, individual or corporate information verified, and then returned as an issued certificate. This key is also used to provide signed responses from the REST endpoints, and thus must have it key usage bits set to digitalSignature, and an extendedKeyUse of emailProtection. The CN of a certficate should be set to admin@dnscatcher.example.com. The first part of the CN is ignored for validation of messages and may be assigned freely.

At the time of client enlistment, the CA signed KSK's PEM certificate is sent to the client, and shows the encoded fields within the cert which include the organization, issuer, city, state, and country, plus the CN for the DNSCatcher domain. DNSCatcher automatically publishes CERT records so the chain can easily be downloaded for verification and to help avoid misissuance.

**NOTE:** CAA records are NOT checked for S/MIME certificates per the BR as of writing. If this changes or a similar mechanism is implemented, it will be added to DNSCatcher ASAP. CAA records shall be added however for standard TLS issuance.

As DNSCatcher clients validate this key through OCSP and/or CRLs, certificate authorities can revoke misissued keys if one is fraudantly obtained from a certificate authority, reducing risk, and increasing the strength of attributation and accountability.

Multiple S/MIME certificates may be used for the cases of multiple servers where sharing of private keys is undesirable; in this case, multiple DS and DNSKEY records must be published in the parent zone. TLSA records are pushed to the "authoritive" Catcher server.

Seperate TLS certificates are used for DoT/DoH services.

### Services

DNSCatcher operates four services, a standard DNS server on port 53, DNS-over-TLS on port 853, DNS over HTTPS and a REST interface on port 443. An optional web interface can also be run against the REST interface. Alternate ports from IANA will be requested so Catcher can be run alongside another server without conflict. Catcher acts as an autherative server for it's own subdomain; for example, given the domain example.org, DNSCatcher should be deployed to dnscatcher.example.org with a NS delegation of the dnscatcher subdomain.

Catcher has the ability to run as a recursive resolver for legacy (normal DNS clients), although this functionality is disabled by default. It can also act as a forwarding DNS server, performing validation and verification in real time for legacy clients. Instead, this DNS server is primarily intended to hold the CHK_IN class, receive zone transfer requests, and manage it's own TLSA signing records. It also hosts the DNSCatcher Bootstrap file which can be used by clients that are otherwise unable to resolve or validate Catcher's server information or for use in private networks.

### First Run
On first run, the server will undertake several initialization steps. This section assuming a publicly reachable DNSCatcher instance; see the section on private networks for steps regarding that setup.

First, it will initialize it's backend database (currently expected to be PostgreSQL), create the initial administrator user, and finally generate a 2048-bit RSA public and private key, and generate a certificate signing request. This keypair will be used for DNSSEC, locally signed RRSIGs/DLV (see below) and a signed X.509 certificate will provide attribution information. This certificate should be signed for S/MIME signatures, and be a OV (Class 2 S/MIME) or better certificate.

A seperate certificate for serverAuth is generated which has the server's hostnames and (ideally) the ipAddress encoded as the subect alternate names. This can be a DV certificate, although it is recommended that it be in the form of OV or EV.

When deployed on the public Internet, DNSCatcher's domain entries MUST be DNSSEC-signed, and TLSA records deployed (Catcher will create it's own TLSA records but requires installation of a X.509 certificate, and additions of DS records to it's parent zone).

Furthermore, the system administrator must set upstream DNS servers (used for validation checking in addition to recursive resolve), forwarding DNS servers (for use within corporate networks, SoHo environments, etc) which handle IN traffic sent to the Catcher server, and upstream DNSCatcher servers if necessary.

In this state, the server is in BOOTSTRAP mode, and providing unsigned DNS services for records it controls. No validation services are available.

### Startup Process
After initial configuration is complete, DNSCatcher will begin a self-check of it's environment to make sure it meets it's own safety constraints. The following information is checked:

 * Check that the database schema versions are compatible
 * KSK updates are checked for via the RFC 5011 mechanism
    * This process happens once a day or at server restart, whichever comes first
 * The server recursively resolves it's own hostname, checking that it can successfully locate itself, and perform a DNSSEC sanity check
   * Server startup is aborted if this check fails
 * A reverse lookup is done for IPv4 and IPv6 addresses to make sure they point back the DNSCatcher instance.
 * The root server zone is downloaded from IANA and loaded to the database to reduce load of the roots. This is updated as per the TTLs.
    * The root zone RRSIGs are enumerated and confirmed to be properly signed
 * A randomly generated TXT record is created.
 * The server hostname is enumerated through each upstream server
    * The server's A/AAAA records are validated
    * The TXT record is validated (this confirms caching behavior of the upstream servers)
    * Each upstream server is validated for correct behavior, or marked 'offline' if the test fails. This is rechecked on regular intervals
 * If forwarder servers are specified, it's enumerated as per the client startup process
 * The standard DNS, DoT, and DoH interfaces are initialized
   * TLSA records are created/updated as necessary
 * A CERT record is published with the KSK signed in X.509 format as descirbed above
   * The server validates the KSK-X509 certificate, confirming it is not expired or revoked. If either case is true, validation services are disabled.

After this process, the server is in RUNNING mode and is fully ready to process client requests.

### DNS Diagnostic Checks
As a design goal, one of many DNSCatcher's features is to provide a health indicator to the state of the recursive resolver infrastructure as used on the Internet. To this end, DNSCatcher clients run a comprehensive set of tests checking the behavior and data returned by recursive resolvers.

To this end, the Catcher server will create a set of authoritive domains and records that test aspects of various RFCs to ensure they're correctly implemented such as CD bit handling, various DNSSEC hashing algrthimns, and RRtype handling. For more information, see the Client section on Resolver Diagonsitics.

This data is collected and processed by the server.

### DNS RR Validation
As DNSCatcher's primary purpose is to provide validation and cross-check of DNS records, the server is primarily entrusted to building safe paths to validate records and return their status to the client as detailed above.

In the simplist of cases, the server can definitively prove that a record is correct; we'll cover these cases first.

#### No External Validation Cases

There are three cases where DNSCatcher can directly attest that a record is correct either from it's own knowledge, or by a single recursive resolve.

##### DNSCatcher is the Authortive Server
For a small subset of requests, DNSCatcher will return AA=1 for records it directly manages. In these cases, Catcher's path validate simply returns authoritive and includes the DNSSEC chain (if available). The validation path simply returns 'authoritive'

##### The Record Set is Signed
If a given RRtype is signed with DNSSEC, and DNSCatcher can successfully retrieve the entire chain, no further validation is required. 'valid-secure' is returned, and processing is ended. The chain is cached. The path returned is 'dnssec-signed'

##### DNSCatcher has a local copy of the zone file
Part of Catcher's design is to import zone files to both reduce load on the root servers and on the TLD domains. TLD zone files are available through ICANN's CZDB, and at a minimium, all Catcher instances can import the root zone, and .arpa. If a record from one of these local copies is requested *and* the record is still valid TTL, DNSCatcher may response with a validation path of 'has-local-zone-file'

#### External Validation Required
In the most common cases however, DNSCatcher needs to do some ground work to determine if a record is actually valid. As Catcher is in of itself a DNS resolver, it is supsectable to the DNS attacks listed at the top of this document. To ensure that Catcher is getting accurate records, a system known as cross-check is used to build a qurom. This involves several steps.

1. The client submits a request to validate a record submitting what it sent as a question and the answer received.

2. The DNSCatcher server performs a recursive resolve from the root to the records designed, utilizing its own internals caches as necessary to speed up processing. In the best case scenario, this information can entirely be pulled from the database with no network traffic required. This forms one half of the quorm. Because DoT/DoH is not present on most DNS servers, this primarily takes the form of unencrypted DNS requests and thus is susceptible to tampering.

3. A cross-check server (from the list set by the server admin) is selected at random, and the client's question is run, and an answer is received. Cross-check servers always use DNS-over-TLS/DNS-over-HTTPS to ensure inflight intergrity. Cross-check servers are simply public DNS recursive resolvers that support DoT/DoH. Where possible, key pinning will be used to further harden these connections. **NOTE:** A client may opt out of choosing to be cross-checked, only depending on Catcher's recursive resolve + any cached records for reasons of privacy.

4. The results of the recursive resolve and the cross-check are compared. If they're equal, and also equal to the client's lookup, the result is marked as valid and returned to the client.

5. If the cross-check fails, but the client's answer matches one of the responses, Catcher can request additional information such as SOA information to attempt to determine if a stale record is in play. If so, valid-stale, or stale-unconfirmed is returned. Each cross-check lookup is stored in the database so the process does not need to repeat for every single lookup

6. If the recursive resolve and the cross-check mismatch (possible due to DNS caching), search continues to the next server on the list up to max-tries. This continues until qurom is reached, or the server list is exhausted. If qurom succeeds with two cross-check servers, a valid response is returned to the client, and a warning is logged that the Catcher server may be under attack.

7. If the recursive resolver, and the cross-check server match each other, but not the client, the server will try to determine if the client is under split-horizon or other cases where a mismatch can occur. If this proves impossible, an invalid/blackhole response is sent.

8. If all data sources return different answers, and quorm can not be reached, DNSCatcher sends up the white flag and returns "unknown".

A full list of operations and paths used to validate records are sent to the client for information purposes.

### Work Units
DNSCatcher servers can propose work units that clients execute to run specific lookups against their local resolvers to provide a deeper picture on the state of the Internet. This mechanism is designed to understand attacks against the DNS ecosystem, as well as provide definitive answers to questions such as TLD name collision concerns.

Because this mechanism can be abused to launch DDoS attacks, identify individual users to third party servers and other mayhem, processing of Work Units is opt-in on the part of the client, and WUs must be signed with a special WU-Signing key before they're kept valid. The WU-Signing key is not stored on the DNSCatcher server for security reason; instead, proposed WUs must be downloaded by a system admin, signed, and uploaded before distributed to willing clients.

WU-Signing keys are indicated by SMIMEA records (proposed DANE extension for S/MIME), and subject to standard expiration and revocation checks.

### Post-Processing

Finally, as part of it's use as a research tool, Catcher performs a number of post-processing steps. This isn't meant as an exhaustive list.

 * Recursive Resolver information is collected from the client, to determine if it using a local resolver, an ISP provided one, or a known public resolver
  * IP address information from clients is run through a GeoIP database to determine ISP information from where a client is connecting from at a given moment
  * Skewed records are identified and highlighted and anazylized for trends (i.e., is ISP A blocking access to website B?)
  * DNS Diagnostic Information is categorized and sorted
  * Work units for client testing is suggested and sent to the system administrators for verification 

Information is stored for an sysadmin defined retention period before being deleted. Raw IP addresses are only retained up to the point of post-processing to preserve user anonymity.

### RFC1918/ULA (Private Network) Deployment

Due to logisitical difficulties relating to WebPKI certificates in a private network, special steps, and modifications of the above must be used in place of the above menthoned deployment mechanisms. This section documents these challenges, and how Catcher can be deployed in these environments.

### Server To Server Operation (S2S)
In certain cases, a DNSCatcher server should be deployed as a slave to a higher server. This is primarily in cases of private network deployment, or when a network endpoint is managing a large number of outgoing standard DNS requests such as a Tor exit node, or VPN endpoint. S2S operation allows information from this hosts to be collected without compromising their anomynity.

(TBD)

## Client Operation

Catcher client software (either in the form of a standalone client, or browser extension) is slaved to a given Catcher server and begins providing telemetry and cross-checking information. Upon initialization, the following steps take place.


### Key Pinning

## Client Interfaces

### HTTP-REST

### DNS CHK_IN Class

#### Caching Resolver Interaction

## Work Unit Interface

### Operations

#### Client DNS Lookup

## Security Concerns

Due to the large amount of data that could be collected by a DNSCatcher instance, an understanding of the risk factors and information collected is extremely important.

Out of the box, DNSCatcher could inadvertently collect the following information. **NOTE:** Much of the security factors listed below are true of DNS recursive resolvers in general and the data logging they generate, and are not unique to the DNSCatcher proect:

### Sensitive domains and record types

It is not uncommon for people to misuse DNS and place far too much information about a given network. This is similar to the concept of zone walking that required the creation of the NSEC3 record in DNSSEC and work towards the NSEC5 standard. Where possible, a compromise between keeping the data set usable, while protecting user privacy must be made and is extremely important.
  * **Possible migations:**
    * Redact information beyond the second or third level domain after a period of time. Said domains can be hashed and salted to provide generalized trends of information while not revealing everything in case of database comprimise. Information can be stored as a hash beyond this point given that second level domain zone files are at least accessible through ICANN.
    * As the cross-check mechanism requires knowing the full DNS label and RRType, it is not possible to directly migate this in the client. It is possible that DNSCurve technology could however be used; further research required.

### Accidental Collection Personally Identifiable Information
See above, although this is less common in DNS as it is in other mediums.


### Leak of split-horizon DNS information

Split-horizon is not considered a good practice by the DNS community, but it's extremely common in practice. Given Catcher needs to determine if it's seeing a split horizon domain, this information has to be sent from the server to the client
  * **Potential Mitigation:** 
    * Determination of split horizon domain names can result it negative caching, or caching with a short TTL to prevent leak of data information. An exception however can be granted in the case of studying name collision issues where long-lived data is extremely valuable for top level domains (while only revealing minimal amounts of information)
    * **NOTE:** As there is no "sure fire" way to determine if split horizon is indeed in affect, some information will still be collected as a result of "invalid" determinations by the DNSCatcher server.

### Collection of vulerable DNS server information

As part of determining the health of the recursive resolver ecosystem, the DNSCatcher client will run a comphrenesive test of the local resolvers abilities, and any responses to the de-facto mechanism of CH bind.version. As such, we may inadvertently collect a large list of site's that are running vulerable or obselete software.

  * **Mitigations:** 
    * We can limit the retention of IP address information of these servers; furthermore, it is expected that most vulerable servers will not be directly accessible from the Internet; and thus not subject to direct attack.
    * We could delete all data except test results after a period of time; reducing the amount of information available incase of breach.

### Work Unit System Can Expose Users

The WU system described above is important for understanding how certain queries are seen from various aspects of the Internet. However, this mechanism is also subject to abuse as detailed above.

  * **Mitigations:**
    * Work Unit functionality is opt-in
    * Signing keys for WUs are not stored as part of the server, and are instead kept seperately. While an attacker could potentially reconfigure the server to change the trusted WU accepted keys, the requirement for class 2 S/MIME certificates issued by a public CA + S/MIME key pinning + requirement of an email address from the DNSCatcher domain (to prevent use of compromised technically constrainted CAs) drastically complicates the attack in such a way that an attacker would have to get a key fradulantly issued by a CA.
      * There are concerned on how robust the CA infrastructure is in identity validation, especially in regards to S/MIME are in this area.