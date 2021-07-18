use std::net::IpAddr;
use std::str::FromStr;
use std::collections::BTreeMap;
use std::fmt;
use url::Host;
use http::uri::Scheme;

#[derive(Debug,Clone,PartialEq)]
pub enum Nodename {
    Ip(IpAddr),
    Obf(String),
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
    Node(Nodename,Nodeport),
    Unknown
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
            let mut s = format!("{}",by);
            s.insert_str(0,";by=");
            s
        } else {
            String::from("")
        };
        let host = if let Some(host) = &self.host {
            let mut s = host.to_string();
            s.insert_str(0,";host=");
            s
        } else {
            String::from("")
        };
        let proto = if let Some(proto) = &self.proto {
            let mut s = format!("{}",proto);
            s.insert_str(0,";proto=");
            s
        } else {
            String::from("")
        };
        let extensions = if self.extensions.len() > 0 {
            let mut i = self.extensions.iter();
            let f = i.next().ok_or(fmt::Error)?;
            let mut s = format!(";{}={}",f.0,f.1);
            while let Some((k,v)) = i.next() {
                let st = format!(";{}={}",k,v);
                s.insert_str(0,st.as_str());
            }
            s
        } else {
            String::from("")
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

impl FromStr for Forwarded {
    type Err = ParseForwardedError;
    //TODO. make it more powerfull
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut v = Vec::new();
        for e in s.split(',') {
            let elm = ForwardedElement::from_str(e)
                .map_err(|_| ParseForwardedError)?;
            v.push(elm);
        }
        return Ok(Forwarded {
            forwarded_element:  v
        });
    }
}

impl fmt::Display for Forwarded {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut i = self.forwarded_element.iter();
        let mut s = String::from("");
        while let Some(f) = i.next() {
            let st = f.to_string();
            s.push_str(&st);
            s.push(',');
        }
        let _ = s.pop();
        write!(f,"{}",s)
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
}
