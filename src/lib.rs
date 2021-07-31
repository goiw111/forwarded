use std::net::IpAddr;
use std::str::FromStr;
use std::collections::BTreeMap;
use url::Host;
use http::uri::Scheme;
use std::fmt;
use std::error::Error;
use std::net::SocketAddr;

#[derive(Debug,Clone,PartialEq)]
pub enum Nodename {
    Ip(IpAddr),
    Obf(String),
}

impl From<IpAddr> for Nodename {
    fn from(ip: IpAddr) -> Self {
        Nodename::Ip(ip)
    }
}

impl From<String> for Nodename {
    fn from(obf: String) -> Self {
        Nodename::Obf(obf)
    }
}

impl fmt::Display for Nodename {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Nodename::Ip(ip)  => {
                match ip {
                    IpAddr::V4(ip) => return write!(f, "{}", ip),
                    IpAddr::V6(ip) => return write!(f, "[{}]", ip),
                }
            },
            Nodename::Obf(s)  => return write!(f, "_{}", s),
        }
    }
}

#[derive(Debug)]
pub struct ParseNodenameError;

impl Error for ParseNodenameError {}

impl fmt::Display for ParseNodenameError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "failed to convert str to a nodename")
    }
}

impl FromStr for Nodename {
    type Err = ParseNodenameError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let t = s.trim_matches(|c| c == '[' || c == ']' || c == '"');
        if let Ok(ip) = t.parse::<IpAddr>() {
             return Ok(Nodename::Ip(ip));
        } else {
            if let Some(x) = get_obf(s) {
                return Ok(Nodename::Obf(x));
            }
        }
        Err(ParseNodenameError)
    }
}

#[derive(Debug,Clone,PartialEq)]
pub enum Nodeport {
    Port(u16),
    Obf(String),
    None
}

impl From<u16> for Nodeport {
    fn from(ui: u16) -> Self {
        Nodeport::Port(ui)
    }
}

impl From<String> for Nodeport {
    fn from(obf: String) -> Self {
        Nodeport::Obf(obf)
    }
}

impl fmt::Display for Nodeport {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Nodeport::Port(n)   => return write!(f, "{}", n),
            Nodeport::Obf(s)    => return write!(f, "_{}", s),
            _                   => return Err(fmt::Error),
        }
    }
}

#[inline]
fn get_obf(s:   &str) -> Option<String> {
    if let Some(x) = s.strip_prefix('_') {
        if x.chars()
            .all(|x| x.is_ascii_alphanumeric() 
                || x == '.' 
                || x == '_' 
                || x == '-') {
                return Some(String::from(x));
        }
    }
    None
}

#[derive(Debug,Clone,PartialEq)]
pub enum Node {
    Node(Nodename, Nodeport),
    Unknown
}

impl From<SocketAddr> for Node {
    fn from(saddr:  SocketAddr) -> Self {
        Node::Node(saddr.ip().into(),saddr.port().into())
    }
}

impl From<Option<SocketAddr>> for Node {
    fn from(saddr:  Option<SocketAddr>) -> Self {
        if let Some(saddr) = saddr {
            return  Node::Node(saddr.ip().into(),saddr.port().into())
        }
        Node::Unknown
    }
}

impl From<IpAddr> for Node {
    fn from(ipadds:  IpAddr) -> Self {
        return  Node::Node(ipadds.into(),Nodeport::None)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Unknown   => return write!(f, "unknown"),
            Node::Node(n,p) => {
                match p {
                    Nodeport::None  => {
                        if let Nodename::Ip(IpAddr::V6(ip)) = n {
                            return write!(f, "\"[{}]\"", ip);
                        }
                        return write!(f, "{}", p);
                    },
                    _               => return write!(f, "\"{}:{}\"", n, p),
                }
            },
        }
    }
}

#[derive(Debug)]
pub struct ParseNodeError;

impl Error for ParseNodeError {}

impl fmt::Display for ParseNodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "failed to convert str to a node")
    }
}

impl FromStr for Node {
    type Err = ParseNodeError;
    //TODO. make it more powerfull
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim_matches('"');
        if s == "unknown" {
            return Ok(Node::Unknown);
        }
        if let Ok(x) = Nodename::from_str(s) {
            return Ok(Node::Node(x,Nodeport::None));
        }
        if let Some((l,r)) = s.rsplit_once(':') {
            let p = if let Ok(x) = r.parse::<u16>() {
                Nodeport::Port(x)
            } else {
                if let Some(x) = get_obf(r) {
                    Nodeport::Obf(x)
                } else {
                    return Err(ParseNodeError);
                }
            };
            if let Ok(x) = Nodename::from_str(l) {
                return Ok(Node::Node(x,p));
            }
        }
        return Err(ParseNodeError);
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct ForwardedElement {
    for_:       Node,
    by:         Option<Node>,
    host:       Option<Host>,
    proto:      Option<Scheme>,
    extensions: BTreeMap<String,String>
}

impl fmt::Display for ForwardedElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let by = if let Some(by) = &self.by {
            format!(";by={}",by)
        } else {
            String::from("")
        };
        let host = if let Some(host) = &self.host {
            format!(";host={}",host)
        } else {
            String::from("")
        };
        let proto = if let Some(proto) = &self.proto {
            format!(";proto={}",proto)
        } else {
            String::from("")
        };
        let extensions = if self.extensions.len() > 0 {
            self.extensions
                .iter()
                .map(|(k,v)| format!(";{}={}",k,v))
                .collect::<String>()
        } else {
            String::new()
        };
        write!(f,"for={for_}{by}{host}{proto}{extensions}",
            for_= self.for_,
            by= by,
            host= host,
            proto= proto ,
            extensions= extensions)
    }
}

impl ForwardedElement {
    pub fn new(node:    Node) -> Self {
        ForwardedElement {
            for_:       node,
            by:         None,
            host:       None,
            proto:      None,
            extensions: BTreeMap::new()
        }
    }
    pub fn set_by(&mut self, node: Node) {
        self.by = Some(node);
    }
    pub fn set_host(&mut self, host: &str) 
        -> Result<(),ParseForwardedElementError> {
        self.host = Some(Host::parse(host)
            .map_err(|_| ParseForwardedElementError)?);
            Ok(())
    }
    pub fn set_proto(&mut self, proto: Scheme) {
        self.proto = Some(proto);
    }
    pub fn set_extensions(&mut self, (key,value): (&str,&str)) {
        self.extensions.insert(String::from(key),String::from(value));
    }
}

#[derive(Debug)]
pub struct ParseForwardedElementError;

impl Error for ParseForwardedElementError {}

impl fmt::Display for ParseForwardedElementError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "failed to convert str to ForwardedElement")
    }
}


impl FromStr for ForwardedElement {
    type Err = ParseForwardedElementError;
    //TODO. make it more powerfull
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let r = s.split(';')
            .map(|x|x.split_once('='));
        if !r.clone().any(|x| x.is_some())
            {return Err(ParseForwardedElementError);}
        let n = r
            .map(Option::unwrap)
            .collect::<Vec<(&str,&str)>>();
        if let Some((f,elm)) = n.split_first() {
            let mut e = if let ("For",r) | ("for",r) = f {
                let n = Node::from_str(r)
                    .map_err(|_| ParseForwardedElementError)?;
                ForwardedElement::new(n)
            } else {
                return Err(ParseForwardedElementError);
            };
            for (l,r) in elm {
                match *l {
                    "by" | "By"         =>{
                        let v = Node::from_str(r)
                            .map_err(|_| ParseForwardedElementError)?;
                        e.set_by(v);
                    },
                    "host" | "Host"     =>{
                        e.set_host(r)?;
                    },
                    "proto" | "Proto"   =>{
                        let v = r.parse::<Scheme>()
                            .map_err(|_| ParseForwardedElementError)?;
                        e.set_proto(v);
                    },
                    _                   =>{
                        let r = r.trim_matches('"');
                        e.set_extensions((l,r));
                    },
                }
            }
            return Ok(e);
        }
        Err(ParseForwardedElementError)
    }
}
#[derive(Debug,Clone,PartialEq)]
pub struct Forwarded {
    forwarded_element:  Vec<ForwardedElement>
}

#[derive(Debug)]
pub struct ParseForwardedError;

impl Error for ParseForwardedError {}

impl fmt::Display for ParseForwardedError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "failed to convert str to a Forwarded")
    }
}


impl FromStr for Forwarded {
    type Err = ParseForwardedError;
    //TODO. make it more powerfull
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut v = Vec::new();
        if s.is_empty() == false {
        for e in s.split(',') {
            let elm = ForwardedElement::from_str(e)
                .map_err(|_| ParseForwardedError)?;
            v.push(elm);
        }}
        return Ok(Forwarded {
            forwarded_element:  v
        });
    }
}

impl fmt::Display for Forwarded {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut i = self.forwarded_element
            .iter()
            .map(|x| format!("{},",x))
            .collect::<String>();
        let _ = i.pop();
        write!(f,"{}",i)
    }
}

impl Forwarded {
    pub fn new() -> Self {
        Forwarded {
            forwarded_element:  Vec::new()
        }
    }
    pub fn set_element(&mut self,elm:  ForwardedElement) {
        self.forwarded_element.push(elm);
    }
    pub fn remove_element(&mut self,index:  usize) -> ForwardedElement {
        self.forwarded_element.remove(index)
    }
    pub fn is_empty(&self) -> bool {
        self.forwarded_element.is_empty()
    }
    pub fn from_utf8(v: &[u8]) -> Result<Self,ParseForwardedError> {
        if let Ok(s) = std::str::from_utf8(v) {
            return Self::from_str(s);
        };
        Err(ParseForwardedError)
    }
}

use actix_web::http::header::{self,Header};
use actix_web::error::ParseError;
use actix_web::HttpMessage;
use actix_web::http::HeaderName;
use actix_web::http::header::IntoHeaderValue;
use actix_web::http::header::InvalidHeaderValue;
use actix_web::http::HeaderValue;
use std::convert::TryFrom;

impl IntoHeaderValue for Forwarded {
    type Error = InvalidHeaderValue;
    #[inline]
    fn try_into(self) -> Result<HeaderValue, Self::Error> {
        HeaderValue::try_from(self.to_string())
    }
}

impl Header for Forwarded {
    fn name() -> HeaderName {
        header::FORWARDED
    }
    fn parse<T: HttpMessage>(msg: &T) -> Result<Self, ParseError> {
        if let Some(hv) = msg.headers().get(Self::name()) {
            return Ok(Self::from_utf8(hv.as_bytes())
                .map_err(|_|ParseError::Header)?);
        }
        Ok(Forwarded::new())
    }
}
//TODO. add more unexpected tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn forwardedelement_for() {
        let n  = Node::from_str("[2001:db8:cafe::17]:154").unwrap();
        let l  = ForwardedElement::new(n);
        let mut f1 = Forwarded::new();
        f1.set_element(l);

        let f2 = Forwarded::from_str(r#"for="[2001:db8:cafe::17]:154""#).unwrap();

        assert_eq!(f1,f2);
    }

    #[test]
    fn forwardedelement_for_by() {
        let n  = Node::from_str("[2001:db8:cafe::17]:154").unwrap();
        let mut l  = ForwardedElement::new(n);
        let n  = Node::from_str("192.168.1.1:80").unwrap();
        l.set_by(n);
        let mut f1 = Forwarded::new();
        f1.set_element(l);

        let f2 = Forwarded::from_str(
            r#"for="[2001:db8:cafe::17]:154";by="192.168.1.1:80""#
            ).unwrap();
        assert_eq!(f1,f2);
    }
    #[test]
    #[warn(unused_must_use)]
    fn forwardedelement_for_by_host() {
        let n  = Node::from_str("[2001:db8:cafe::17]:154").unwrap();
        let mut l  = ForwardedElement::new(n);
        let n  = Node::from_str("_something:_something").unwrap();
        l.set_by(n);
        l.set_host("google.com").unwrap();
        let mut f1 = Forwarded::new();
        f1.set_element(l);

        let f2 = Forwarded::from_str(
            r#"for="[2001:db8:cafe::17]:154";by="_something:_something";host=google.com"#
            ).unwrap();
        assert_eq!(f1,f2);
    }
    #[test]
    fn forwardedelement_for_by_host_proto() {
        let n  = Node::from_str("[2001:db8:cafe::17]:154").unwrap();
        let mut l  = ForwardedElement::new(n);
        let n  = Node::from_str("unknown").unwrap();
        l.set_by(n);
        l.set_host("google.com").unwrap();
        l.set_proto(Scheme::HTTP);
        let mut f1 = Forwarded::new();
        f1.set_element(l);

        let f2 = Forwarded::from_str(
            r#"for="[2001:db8:cafe::17]:154";by=unknown;host=google.com;proto=http"#
            ).unwrap();
        assert_eq!(f1,f2);
    }

    #[test]
    fn forwardedelement_for_by_host_proto_ext() {
        let n  = Node::from_str("[2001:db8:cafe::17]:154").unwrap();
        let mut l  = ForwardedElement::new(n);
        let n  = Node::from_str("192.168.1.1").unwrap();
        l.set_by(n);
        l.set_host("google.com").unwrap();
        l.set_proto(Scheme::HTTP);
        l.set_extensions(("secret","fsdfsdfs"));
        l.set_extensions(("desc","some desc"));
        let mut f1 = Forwarded::new();
        f1.set_element(l);

        let f2 = Forwarded::from_str(
            r#"for="[2001:db8:cafe::17]:154";by=192.168.1.1;host=google.com;proto=http;secret="fsdfsdfs;desc="some desc""#
        ).unwrap();
        assert_eq!(f1,f2);
    }
    #[test]
    fn forwardedelements_for_by_host_proto_ext() {
        let n  = Node::from_str("[2001:db8:cafe::17]").unwrap();
        let mut l1  = ForwardedElement::new(n);
        let n  = Node::from_str("192.168.1.1:80").unwrap();
        l1.set_by(n);
        l1.set_host("google.com").unwrap();
        l1.set_extensions(("secret","fsdfsdfs"));
        l1.set_proto(Scheme::HTTP);
        l1.set_extensions(("desc","some desc"));
        let n  = Node::from_str("192.168.1.1:80").unwrap();
        let l2  = ForwardedElement::new(n);
        let mut f1 = Forwarded::new();
        f1.set_element(l1);
        f1.set_element(l2);

        let f2 = Forwarded::from_str(
            r#"for="[2001:db8:cafe::17]";by="192.168.1.1:80";host=google.com;proto=http;secret="fsdfsdfs;desc="some desc",for="192.168.1.1:80""#
            ).unwrap();
        assert_eq!(f1,f2);
    }
    #[test]
    fn empty_forwarded() {
        let f = Forwarded::from_str("").unwrap();
        assert_eq!(f.is_empty(),true);
    }

    use actix_web::{http, test};
    use actix_web::HttpRequest;
    use actix_web::HttpResponse;

    async fn index(req: HttpRequest) -> HttpResponse {
        let mut forwarded  = Forwarded::parse(&req).unwrap();
        if let Some(sadds) = req.peer_addr() {
            let fe = ForwardedElement::new(sadds.ip().into());
            forwarded.set_element(fe);
        }
        HttpResponse::Ok()
            .set_header(http::header::FORWARDED,forwarded)
            .finish()
    }

    #[actix_rt::test]
    async fn impl_from_ipaddrs() {
        let req = test::TestRequest::with_header("content-type", "text/plain").to_http_request();
        let resp = index(req).await;
        assert_eq!(resp.status(), http::StatusCode::OK);
    }
}
