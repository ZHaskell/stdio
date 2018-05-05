package main

import (
	"fmt"
	"net"
	"os"
)

const (
	CONN_HOST = "localhost"
	CONN_TYPE = "tcp"
)

func main() {
	// Listen for incoming connections.
	port := "8888"
	if os.Getenv("PORT") != "" {
		port = os.Getenv("PORT")
	}
	l, err := net.Listen(CONN_TYPE, CONN_HOST+":"+port)
	if err != nil {
		fmt.Println("Error listening:", err.Error())
		os.Exit(1)
	}
	// Close the listener when the application closes.
	defer l.Close()
	fmt.Println("Listening on " + CONN_HOST + ":" + port)
	for {
		// Listen for an incoming connection.
		conn, err := l.Accept()
		if err != nil {
			fmt.Println("Error accepting: ", err.Error())
			os.Exit(1)
		}
		// Handle connections in a new goroutine.
		go handleRequest(conn)
	}
}

// Handles incoming requests.
func handleRequest(conn net.Conn) {
	// Make a buffer to hold incoming data.
	buf := make([]byte, 2048)

	for {
		// Read the incoming connection into the buffer.
		_, err := conn.Read(buf)
		if err == nil {
			// Send a response back to person contacting us.
			conn.Write([]byte(
				"HTTP/1.1 200 OK\r\n" +
					"Content-Type: text/html; charset=UTF-8\r\n" +
					"Content-Length: 500\r\n" +
					"Connection: Keep-Alive\r\n\r\n" + "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"))
		} else {
			// Close the connection when you're done with it.
			conn.Close()
			break
		}
	}
}
