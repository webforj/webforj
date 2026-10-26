package com.webforj.testbed.runtime;

import com.basis.startup.type.BBjException;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * The mock backing one file of the client, holding the bytes the browser would hold.
 *
 * <p>
 * Driven by {@code com.webforj.UploadedFile}, which reads the size of an uploaded file, copies it
 * to the server and deletes it, and by {@code com.webforj.Page}, which sends a server file to the
 * client for download.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class BBjClientFileMock extends RuntimeMock {
  private static final String COPY_PREFIX = "clientfs";
  private static final String COPY_SUFFIX = ".tmp";
  private final BBjClientFileSystemMock fileSystem;
  private final String name;
  private byte[] contents;
  private boolean cached;

  /**
   * Creates a new client file mock.
   *
   * @param fileSystem the file system the file belongs to
   * @param name the name of the file on the client
   */
  BBjClientFileMock(BBjClientFileSystemMock fileSystem, String name) {
    this.fileSystem = fileSystem;
    this.name = name;
  }

  /**
   * Stores the bytes of the file.
   *
   * @param contents the bytes, copied into the file
   */
  void setContents(byte[] contents) {
    this.contents = contents.clone();
    cached = true;
  }

  /**
   * Forgets the bytes of the file and that it ever held any.
   */
  void clear() {
    contents = null;
    cached = false;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "exists", "isFile", "canRead" -> contents != null;
      case "size" -> contents == null ? 0L : (long) contents.length;
      case "getContents" -> requireContents().clone();
      case "setContents" -> {
        // The runtime stores the bytes and sends them to the browser as a download.
        setContents((byte[]) args[0]);
        fileSystem.addDownload(name, contents);
        yield null;
      }
      case "delete" -> {
        boolean existed = contents != null;
        contents = null;
        yield existed;
      }
      case "getName", "getPath" -> name;
      case "copyFromClient" -> copyToServer(args.length == 0 ? null : (String) args[0]);
      case "copyToClient" -> {
        copyToClient((String) args[0]);
        yield null;
      }
      default -> UNHANDLED;
    };
  }

  private byte[] requireContents() throws BBjException {
    if (contents == null) {
      throw missing();
    }

    return contents;
  }

  private BBjException missing() {
    return new BBjException("Client file not found: " + name, BBjException.DOM_FILE);
  }

  private String copyToServer(String destination) throws BBjException {
    if (!cached) {
      throw missing();
    }

    Path target;
    OutputStream output;
    try {
      if (destination == null) {
        target = Files.createTempFile(fileSystem.getCacheDirectory(), COPY_PREFIX, COPY_SUFFIX);
        output = Files.newOutputStream(target);
      } else if (Files.isDirectory(Path.of(destination))) {
        target = Files.createTempFile(Path.of(destination), COPY_PREFIX, COPY_SUFFIX);
        output = Files.newOutputStream(target);
      } else {
        // The runtime never overwrites a file the application already has.
        target = Path.of(destination);
        output =
            Files.newOutputStream(target, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
      }
    } catch (IOException | InvalidPathException e) {
      throw new BBjException(e, BBjException.ACCESS);
    }

    // The runtime creates the destination before it reads the client, so a file deleted on the
    // client leaves an empty destination behind.
    try (output) {
      if (contents == null) {
        throw new IOException("Client file was deleted: " + name);
      }

      output.write(contents);
    } catch (IOException e) {
      throw new BBjException(e, BBjException.IO_ERR);
    }

    return target.toAbsolutePath().toString();
  }

  // The runtime reads the server file, hands its bytes to the client under the name of this file
  // and then asks the browser to download it.
  private void copyToClient(String source) throws BBjException {
    byte[] bytes;
    try {
      bytes = Files.readAllBytes(Path.of(source));
    } catch (NoSuchFileException | InvalidPathException e) {
      throw new BBjException(e, BBjException.DOM_FILE);
    } catch (IOException e) {
      throw new BBjException(e, BBjException.IO_ERR);
    }

    setContents(bytes);
    fileSystem.addDownload(name, bytes);
  }
}
