package com.webforj.component.upload.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjFileChooserChangeEvent;
import com.basis.startup.type.BBjVector;
import com.webforj.component.upload.Upload;
import com.webforj.component.upload.event.UploadChangeEvent;
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
class UploadChangeEventSinkTest {

  @Mock
  BBjFileChooserChangeEvent bbjEvent;

  Upload component;
  EventDispatcher dispatcher;
  UploadChangeEventSink sink;
  AtomicReference<UploadChangeEvent> captured;

  @BeforeEach
  void setUp() {
    component = new Upload();
    dispatcher = new EventDispatcher();
    sink = new UploadChangeEventSink(component, dispatcher);
    captured = new AtomicReference<>();
    dispatcher.addListener(UploadChangeEvent.class, captured::set);
  }

  @ParameterizedTest
  @MethodSource("decodeCases")
  void shouldDecodeAndDispatchTypedEvent(BBjVector input, String[] expectedNames) {
    when(bbjEvent.getSelectedFiles()).thenReturn(input);

    sink.handleEvent(bbjEvent);

    UploadChangeEvent dispatched = captured.get();
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
