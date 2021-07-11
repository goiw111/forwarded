use std::net::IpAddr;
use std::str::FromStr;
use http::uri::{Scheme, Authority};
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug,Clone)]
enum Nodename {
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

#[derive(Debug,Clone)]
enum Nodeport {
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

#[derive(Debug,Clone)]
enum Node {
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

pub struct ParseNodeError;

impl FromStr for Node {
    type Err = ParseNodeError;

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

#[derive(Debug)]
struct ForwardedElement {
    for_:       Node,
    by:         Option<Node>,
    host:       Option<String>,
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
            let mut s = host.clone();
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
            let s = format!(";{}={}",f.0,f.1);
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
    fn new(node:    Node) -> Self {
        ForwardedElement {
            for_:       node,
            by:         None,
            host:       None,
            proto:      None,
            extensions: BTreeMap::new()
        }
    }
    fn set_by(&mut self, node: Node) {
        self.by = Some(node);
    }
    fn set_host(&mut self, host: &str) {
        self.host = Some(String::from(host));
    }
    fn set_proto(&mut self, proto: Scheme) {
        self.proto = Some(proto);
    }
    fn set_extensions(&mut self, (key,value): (&str,&str)) {
        self.extensions.insert(String::from(key),String::from(value));
    }
}

pub struct ParseForwardedElementError;

impl FromStr for ForwardedElement {
    type Err = ParseForwardedElementError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let n = s.split(';')
            .map(|x|x.split_once('='))
            .collect::<Vec<Option<(&str,&str)>>>();
        if n.iter().any(|x| x.is_some()) 
        {return Err(ParseForwardedElementError);}
        if let Some((f,elm)) = n.split_first() {
            let mut e = if let Some(("For",r)) | Some(("for",r)) = f {
                let n = Node::from_str(r)
                    .map_err(|_| ParseForwardedElementError)?;
                ForwardedElement::new(n)
            } else {
                return Err(ParseForwardedElementError);
            };
            while let Some(Some((l,r))) = elm.iter().next() {
                match *l {
                    "by" | "By"         =>{
                        let v = Node::from_str(r)
                            .map_err(|_| ParseForwardedElementError)?;
                        e.set_by(v);
                    },
                    "host" | "Host"     =>{
                        let v = r.parse::<Authority>()
                            .map_err(|_| ParseForwardedElementError)?;
                        e.set_host(v.host());
                        },
                    "proto" | "Proto"   =>{
                        let v = r.parse::<Scheme>()
                            .map_err(|_| ParseForwardedElementError)?;
                        e.set_proto(v);
                    },
                    _                   =>{
                        e.set_extensions((l,r));
                    },
                }
            }
        }
        Err(ParseForwardedElementError)
    }
}
#[derive(Debug)]
pub struct Forwarded {
    forwarded_element:  Vec<ForwardedElement>
}

pub struct ParseForwardedError;

impl FromStr for Forwarded {
    type Err = ParseForwardedError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut v = Vec::new();
        while let Some(e) = s.split(',').next() {
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
