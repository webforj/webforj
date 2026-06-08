package com.webforj.plugin.foundation;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A small line oriented TCP server the watch uses to forward its output to the application.
 *
 * <p>
 * The server binds a free port on the loopback interface chosen by the operating system, then
 * accepts application connections and pushes every line to all of them. The application reconnects
 * after each development restart, and the previous connection from the discarded application drops
 * on its own, so broadcasting to whatever is currently connected needs no coordination with the
 * restart. The server never reads from the application beyond accepting the connection.
 * </p>
 *
 * <p>
 * Lines sent before any application has connected are held and replayed to the first one that does,
 * so the output of the initial build reaches the application log rather than being lost. The server
 * never reads from the application beyond accepting the connection.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class WatchSocketServer implements AutoCloseable {

  private static final int MAX_PENDING_LINES = 2000;

  private final ServerSocket serverSocket;
  private final Set<Socket> clients = ConcurrentHashMap.newKeySet();
  private final Object writeLock = new Object();
  private final Thread acceptThread;
  private final List<String> pending = new ArrayList<>();
  private boolean replayed = false;

  /**
   * Opens the server on a free loopback port chosen by the operating system.
   *
   * @throws IOException when the port cannot be opened
   */
  public WatchSocketServer() throws IOException {
    this.serverSocket = new ServerSocket(0, 50, InetAddress.getLoopbackAddress());
    this.acceptThread = new Thread(this::acceptLoop, "webforj-watch-accept");
    this.acceptThread.setDaemon(true);
  }

  /**
   * Gets the free port the server is listening on.
   *
   * @return the listening port
   */
  public int getPort() {
    return serverSocket.getLocalPort();
  }

  /**
   * Starts accepting application connections on a daemon thread.
   */
  public void start() {
    acceptThread.start();
  }

  private void acceptLoop() {
    while (!serverSocket.isClosed()) {
      try {
        register(serverSocket.accept());
      } catch (IOException e) {
        if (serverSocket.isClosed()) {
          return;
        }
      }
    }
  }

  private void register(Socket client) {
    synchronized (writeLock) {
      clients.add(client);
      // The first application to connect receives whatever was logged before it was up, such as the
      // initial build, so the whole watch output reaches the application log.
      if (!replayed) {
        replayed = true;
        for (String line : pending) {
          writeTo(client, line);
        }

        pending.clear();
      }
    }
  }

  /**
   * Sends a single line to every connected application, dropping the dead ones.
   *
   * <p>
   * Before any application has connected the line is buffered and replayed to the first one that
   * connects.
   * </p>
   *
   * @param line the line to send, without a trailing newline
   */
  public void send(String line) {
    synchronized (writeLock) {
      if (clients.isEmpty() && !replayed) {
        pending.add(line);
        if (pending.size() > MAX_PENDING_LINES) {
          pending.remove(0);
        }

        return;
      }

      for (Socket client : clients) {
        writeTo(client, line);
      }
    }
  }

  private void writeTo(Socket client, String line) {
    try {
      OutputStream out = client.getOutputStream();
      out.write((line + "\n").getBytes(StandardCharsets.UTF_8));
      out.flush();
    } catch (IOException e) {
      clients.remove(client);
      closeQuietly(client);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void close() {
    // Close the server socket first so the accept loop stops registering new clients, then drain
    // whatever is connected.
    closeQuietly(serverSocket);
    synchronized (writeLock) {
      for (Socket client : clients) {
        closeQuietly(client);
      }

      clients.clear();
      pending.clear();
    }
  }

  private static void closeQuietly(AutoCloseable closeable) {
    try {
      closeable.close();
    } catch (Exception e) {
      // a closing failure on a dead socket has nothing left to recover
    }
  }
}
