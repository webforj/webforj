package com.webforj.component.fileupload.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjFileChooserFilterEvent;
import com.webforj.component.fileupload.FileUpload;
import com.webforj.component.fileupload.event.FileUploadFilterChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
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
class FileUploadFilterChangeEventSinkTest {

  @Mock
  BBjFileChooserFilterEvent bbjEvent;

  FileUpload component;
  EventDispatcher dispatcher;
  FileUploadFilterChangeEventSink sink;
  AtomicReference<FileUploadFilterChangeEvent> captured;

  @BeforeEach
  void setUp() {
    component = new FileUpload();
    dispatcher = new EventDispatcher();
    sink = new FileUploadFilterChangeEventSink(component, dispatcher);
    captured = new AtomicReference<>();
    dispatcher.addListener(FileUploadFilterChangeEvent.class, captured::set);
  }

  @ParameterizedTest
  @MethodSource("filterCases")
  void shouldExposeActiveFilter(String bbjReturn, String expected) {
    when(bbjEvent.getActiveFileFilter()).thenReturn(bbjReturn);

    sink.handleEvent(bbjEvent);

    assertNotNull(captured.get());
    assertEquals(expected, captured.get().getActiveFilter());
  }

  static Stream<Arguments> filterCases() {
    return Stream.of(Arguments.of("Text Files", "Text Files"), Arguments.of(null, ""));
  }
}
