package com.webforj.spring.devtools;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.boot.devtools.classpath.ClassPathChangedEvent;
import org.springframework.boot.devtools.filewatch.ChangedFile;
import org.springframework.boot.devtools.filewatch.ChangedFiles;

class DevToolsResourceChangeListenerTest {

  private DevToolsResourceChangeListener listener;
  private DevToolsServer mockServer;
  private ClassPathChangedEvent mockEvent;

  @BeforeEach
  void setUp() {
    listener = new DevToolsResourceChangeListener();
    mockServer = mock(DevToolsServer.class);
    mockEvent = mock(ClassPathChangedEvent.class);
  }

  @ParameterizedTest
  @MethodSource("fileChangeScenarios")
  void shouldProcessFileChangesInStaticDirectory(String filePath, String expectedType,
      String expectedPath, ChangedFile.Type changeType) {
    try (MockedStatic<DevToolsState> mockedState = Mockito.mockStatic(DevToolsState.class)) {
      mockedState.when(DevToolsState::getWebSocketServer).thenReturn(mockServer);
      when(mockServer.isOpen()).thenReturn(true);

      File file = new File(filePath);
      ChangedFile changedFile = mock(ChangedFile.class);
      when(changedFile.getFile()).thenReturn(file);
      when(changedFile.getType()).thenReturn(changeType);

      ChangedFiles changedFiles = new ChangedFiles(file.getParentFile(), Set.of(changedFile));
      when(mockEvent.getChangeSet()).thenReturn(Set.of(changedFiles));

      listener.onApplicationEvent(mockEvent);
      verify(mockServer).sendResourceUpdateMessage(eq(expectedType), eq(expectedPath), isNull());
    }
  }

  @Test
  void shouldProcessMultipleFileChangesInSingleEvent() {
    try (MockedStatic<DevToolsState> mockedState = Mockito.mockStatic(DevToolsState.class)) {
      mockedState.when(DevToolsState::getWebSocketServer).thenReturn(mockServer);
      when(mockServer.isOpen()).thenReturn(true);

      File pngFile = new File("/build/resources/main/static/images/logo.png");
      File svgFile = new File("/build/resources/main/static/icons/icon.svg");

      ChangedFile changedPng = mock(ChangedFile.class);
      when(changedPng.getFile()).thenReturn(pngFile);
      when(changedPng.getType()).thenReturn(ChangedFile.Type.MODIFY);

      ChangedFile changedSvg = mock(ChangedFile.class);
      when(changedSvg.getFile()).thenReturn(svgFile);
      when(changedSvg.getType()).thenReturn(ChangedFile.Type.ADD);

      ChangedFiles changedFiles1 = new ChangedFiles(pngFile.getParentFile(), Set.of(changedPng));
      ChangedFiles changedFiles2 = new ChangedFiles(svgFile.getParentFile(), Set.of(changedSvg));
      when(mockEvent.getChangeSet()).thenReturn(Set.of(changedFiles1, changedFiles2));

      listener.onApplicationEvent(mockEvent);
      verify(mockServer).sendResourceUpdateMessage(eq("image"), eq("images/logo.png"), isNull());
      verify(mockServer).sendResourceUpdateMessage(eq("image"), eq("icons/icon.svg"), isNull());
    }
  }

  @ParameterizedTest
  @MethodSource("nonStaticFileScenarios")
  void shouldIgnoreNonStaticFileChanges(String filePath, ChangedFile.Type changeType) {
    try (MockedStatic<DevToolsState> mockedState = Mockito.mockStatic(DevToolsState.class)) {
      mockedState.when(DevToolsState::getWebSocketServer).thenReturn(mockServer);
      when(mockServer.isOpen()).thenReturn(true);

      File file = new File(filePath);
      ChangedFile changedFile = mock(ChangedFile.class);
      when(changedFile.getFile()).thenReturn(file);
      when(changedFile.getType()).thenReturn(changeType);

      ChangedFiles changedFiles = new ChangedFiles(file.getParentFile(), Set.of(changedFile));
      when(mockEvent.getChangeSet()).thenReturn(Set.of(changedFiles));

      listener.onApplicationEvent(mockEvent);
      verify(mockServer, never()).sendResourceUpdateMessage(any(), any(), any());
    }
  }

  @ParameterizedTest
  @EnumSource(ChangedFile.Type.class)
  void shouldProcessAllChangeTypesForStaticFiles(ChangedFile.Type changeType) {
    try (MockedStatic<DevToolsState> mockedState = Mockito.mockStatic(DevToolsState.class)) {
      mockedState.when(DevToolsState::getWebSocketServer).thenReturn(mockServer);
      when(mockServer.isOpen()).thenReturn(true);

      File file = new File("/static/test.css");
      ChangedFile changedFile = mock(ChangedFile.class);
      when(changedFile.getFile()).thenReturn(file);
      when(changedFile.getType()).thenReturn(changeType);

      ChangedFiles changedFiles = new ChangedFiles(file.getParentFile(), Set.of(changedFile));
      when(mockEvent.getChangeSet()).thenReturn(Set.of(changedFiles));

      listener.onApplicationEvent(mockEvent);
      verify(mockServer).sendResourceUpdateMessage(eq("css"), eq("test.css"), isNull());
    }
  }

  @Test
  void shouldSkipProcessingWhenWebSocketServerNotAvailable() {
    try (MockedStatic<DevToolsState> mockedState = Mockito.mockStatic(DevToolsState.class)) {
      mockedState.when(DevToolsState::getWebSocketServer).thenReturn(null);
      File file = new File("/static/style.css");
      ChangedFile changedFile = mock(ChangedFile.class);
      when(changedFile.getFile()).thenReturn(file);
      when(changedFile.getType()).thenReturn(ChangedFile.Type.MODIFY);

      ChangedFiles changedFiles = new ChangedFiles(file.getParentFile(), Set.of(changedFile));
      when(mockEvent.getChangeSet()).thenReturn(Set.of(changedFiles));

      listener.onApplicationEvent(mockEvent);
      verify(mockServer, never()).sendResourceUpdateMessage(any(), any(), any());
    }
  }

  @Test
  void shouldSkipProcessingWhenWebSocketServerClosed() {
    try (MockedStatic<DevToolsState> mockedState = Mockito.mockStatic(DevToolsState.class)) {
      mockedState.when(DevToolsState::getWebSocketServer).thenReturn(mockServer);
      when(mockServer.isOpen()).thenReturn(false);
      File file = new File("/static/app.js");
      ChangedFile changedFile = mock(ChangedFile.class);
      when(changedFile.getFile()).thenReturn(file);
      when(changedFile.getType()).thenReturn(ChangedFile.Type.ADD);

      ChangedFiles changedFiles = new ChangedFiles(file.getParentFile(), Set.of(changedFile));
      when(mockEvent.getChangeSet()).thenReturn(Set.of(changedFiles));

      listener.onApplicationEvent(mockEvent);
      verify(mockServer, never()).sendResourceUpdateMessage(any(), any(), any());
    }
  }

  @Test
  void shouldExtractCorrectPathFromNestedStaticDirectories() {
    try (MockedStatic<DevToolsState> mockedState = Mockito.mockStatic(DevToolsState.class)) {
      mockedState.when(DevToolsState::getWebSocketServer).thenReturn(mockServer);
      when(mockServer.isOpen()).thenReturn(true);
      File file = new File("/project/static-assets/build/static/css/main.css");
      ChangedFile changedFile = mock(ChangedFile.class);
      when(changedFile.getFile()).thenReturn(file);
      when(changedFile.getType()).thenReturn(ChangedFile.Type.MODIFY);

      ChangedFiles changedFiles = new ChangedFiles(file.getParentFile(), Set.of(changedFile));
      when(mockEvent.getChangeSet()).thenReturn(Set.of(changedFiles));

      listener.onApplicationEvent(mockEvent);
      verify(mockServer).sendResourceUpdateMessage(eq("css"), eq("css/main.css"), isNull());
    }
  }

  @Test
  void shouldHandleEmptyChangeSet() {
    try (MockedStatic<DevToolsState> mockedState = Mockito.mockStatic(DevToolsState.class)) {
      mockedState.when(DevToolsState::getWebSocketServer).thenReturn(mockServer);
      when(mockServer.isOpen()).thenReturn(true);
      when(mockEvent.getChangeSet()).thenReturn(Collections.emptySet());

      listener.onApplicationEvent(mockEvent);
      verify(mockServer, never()).sendResourceUpdateMessage(any(), any(), any());
    }
  }

  private static Stream<Arguments> fileChangeScenarios() {
    return Stream.of(
        Arguments.of("/project/src/main/resources/static/css/style.css", "css", "css/style.css",
            ChangedFile.Type.MODIFY),
        Arguments.of("/app/target/classes/static/js/app.js", "js", "js/app.js",
            ChangedFile.Type.ADD),
        Arguments.of("/build/resources/main/static/images/logo.png", "image", "images/logo.png",
            ChangedFile.Type.MODIFY),
        Arguments.of("/build/resources/main/static/icons/icon.svg", "image", "icons/icon.svg",
            ChangedFile.Type.ADD),
        Arguments.of("/app/static/data/config.json", "other", "data/config.json",
            ChangedFile.Type.MODIFY),
        Arguments.of("/static/old-style.css", "css", "old-style.css", ChangedFile.Type.DELETE));
  }


  private static Stream<Arguments> nonStaticFileScenarios() {
    return Stream.of(Arguments.of("/src/main/java/com/example/App.java", ChangedFile.Type.MODIFY),
        Arguments.of("/src/main/resources/application.xml", ChangedFile.Type.ADD),
        Arguments.of("/src/test/java/com/example/AppTest.java", ChangedFile.Type.DELETE),
        Arguments.of("/pom.xml", ChangedFile.Type.MODIFY));
  }
}
