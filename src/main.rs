use std::net::{TcpListener, TcpStream};

fn handle_client(_stream: TcpStream) {
    //stream.write("ok bye");
}

// Starting from: https://doc.rust-lang.org/std/net/struct.TcpListener.html
fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:3000")?;

    // accept connections and process them serially
    for stream in listener.incoming() {
        handle_client(stream?);
    }
    Ok(())
}
