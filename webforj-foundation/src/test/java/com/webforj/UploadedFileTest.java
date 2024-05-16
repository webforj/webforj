package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjClientFile;
import com.basis.bbj.proxies.BBjClientFileSystem;
import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import java.io.File;
import java.io.IOException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class UploadedFileTest {
  static String FILE_NAME = "example.txt";
  static String SERVER_FILE = "new/path/newfile.txt";

  Environment environment;
  BBjAPI api;
  BBjThinClient thinClient;
  BBjClientFileSystem clientFileSystem;
  BBjClientFile clientFile;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    thinClient = mock(BBjThinClient.class);
    clientFileSystem = mock(BBjClientFileSystem.class);
    clientFile = mock(BBjClientFile.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(api.getThinClient()).thenReturn(thinClient);
    when(thinClient.getClientFileSystem()).thenReturn(clientFileSystem);
    when(clientFileSystem.getClientFile(FILE_NAME)).thenReturn(clientFile);
    when(clientFile.copyFromClient()).thenReturn(SERVER_FILE);
    when(clientFile.copyFromClient(anyString())).thenReturn(SERVER_FILE);
  }

  @Test
  void shouldGetClientFileName() {
    UploadedFile uploadedFile = new UploadedFile(FILE_NAME);
    assertEquals(FILE_NAME, uploadedFile.getClientName());
  }

  @Test
  void shouldGetClientExtension() {
    assertEquals("txt", new UploadedFile("/path/foo.txt").getClientExtension());
    assertEquals("", new UploadedFile(".").getClientExtension());
    assertEquals("", new UploadedFile("..").getClientExtension());
    assertEquals("exe", new UploadedFile("/path/run.exe").getClientExtension());
    assertEquals("", new UploadedFile("/path/makefile").getClientExtension());
    assertEquals("htaccess", new UploadedFile("/path/.htaccess").getClientExtension());
    assertEquals("gz", new UploadedFile("/path/.tar.gz").getClientExtension());
    assertEquals("", new UploadedFile("/path/../makefile").getClientExtension());
    assertEquals("", new UploadedFile("/path/dir.test/makefile").getClientExtension());
  }

  @Test
  void shouldMoveFile() throws IOException {
    UploadedFile uploadedFile = spy(new UploadedFile(FILE_NAME));
    when(uploadedFile.getEnvironment()).thenReturn(environment);

    File result = uploadedFile.move();
    assertNotNull(result);
    assertEquals(SERVER_FILE, result.getPath());
  }

  @Test
  void shouldMoveFileWithNewName() throws IOException {
    UploadedFile uploadedFile = spy(new UploadedFile(FILE_NAME));
    when(uploadedFile.getEnvironment()).thenReturn(environment);

    File result = uploadedFile.move("newfile.txt");
    assertNotNull(result);
    assertEquals(SERVER_FILE, result.getPath());
  }

  @Test
  void shouldThrowExceptionWhenFailedToMoveFile() throws BBjException {
    UploadedFile uploadedFile = spy(new UploadedFile(FILE_NAME));
    when(uploadedFile.getEnvironment()).thenReturn(environment);

    when(clientFile.copyFromClient(anyString())).thenThrow(BBjException.class);

    assertThrows(IOException.class, () -> {
      uploadedFile.move();
    });
  }
}
