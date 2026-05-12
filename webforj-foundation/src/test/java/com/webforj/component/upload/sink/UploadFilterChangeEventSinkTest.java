package com.webforj.component.upload.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjFileChooserFilterEvent;
import com.webforj.component.upload.Upload;
import com.webforj.component.upload.event.UploadFilterChangeEvent;
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
class UploadFilterChangeEventSinkTest {

  @Mock
  BBjFileChooserFilterEvent bbjEvent;

  Upload component;
  EventDispatcher dispatcher;
  UploadFilterChangeEventSink sink;
  AtomicReference<UploadFilterChangeEvent> captured;

  @BeforeEach
  void setUp() {
    component = new Upload();
    dispatcher = new EventDispatcher();
    sink = new UploadFilterChangeEventSink(component, dispatcher);
    captured = new AtomicReference<>();
    dispatcher.addListener(UploadFilterChangeEvent.class, captured::set);
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
