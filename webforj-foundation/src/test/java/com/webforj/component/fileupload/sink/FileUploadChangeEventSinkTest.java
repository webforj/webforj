package com.webforj.component.fileupload.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjFileChooserChangeEvent;
import com.basis.startup.type.BBjVector;
import com.webforj.component.fileupload.FileUpload;
import com.webforj.component.fileupload.event.FileUploadChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class FileUploadChangeEventSinkTest {

  @Mock
  BBjFileChooserChangeEvent bbjEvent;

  FileUpload component;
  EventDispatcher dispatcher;
  FileUploadChangeEventSink sink;
  AtomicReference<FileUploadChangeEvent> captured;

  @BeforeEach
  void setUp() {
    component = new FileUpload();
    dispatcher = new EventDispatcher();
    sink = new FileUploadChangeEventSink(component, dispatcher);
    captured = new AtomicReference<>();
    dispatcher.addListener(FileUploadChangeEvent.class, captured::set);
  }

  @ParameterizedTest
  @MethodSource("decodeCases")
  void shouldDecodeAndDispatchTypedEvent(BBjVector input, String[] expectedNames) {
    when(bbjEvent.getSelectedFiles()).thenReturn(input);

    sink.handleEvent(bbjEvent);

    FileUploadChangeEvent dispatched = captured.get();
    assertNotNull(dispatched);
    assertEquals(expectedNames.length, dispatched.getFiles().size());

    for (int i = 0; i < expectedNames.length; i++) {
      assertEquals(expectedNames[i], dispatched.getFiles().get(i).getClientName());
    }
  }

  static Stream<Arguments> decodeCases() {
    return Stream.of(
        Arguments.of(new BBjVector(Arrays.asList("a.txt", "b.png")),
            new String[] {"a.txt", "b.png"}),
        Arguments.of(null, new String[] {}), Arguments.of(
            new BBjVector(Arrays.asList("a.txt", null, "c.txt")), new String[] {"a.txt", "c.txt"}));
  }
}
