package com.webforj.component.upload.sink;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.basis.bbj.proxies.event.BBjFileChooserCancelEvent;
import com.webforj.component.upload.Upload;
import com.webforj.component.upload.event.UploadCancelEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class UploadCancelEventSinkTest {

  @Mock
  BBjFileChooserCancelEvent bbjEvent;

  Upload component;
  EventDispatcher dispatcher;
  UploadCancelEventSink sink;
  AtomicReference<UploadCancelEvent> captured;

  @BeforeEach
  void setUp() {
    component = new Upload();
    dispatcher = new EventDispatcher();
    sink = new UploadCancelEventSink(component, dispatcher);
    captured = new AtomicReference<>();
    dispatcher.addListener(UploadCancelEvent.class, captured::set);
  }

  @Test
  void shouldDispatchTypedEventWithComponentSource() {
    sink.handleEvent(bbjEvent);

    assertNotNull(captured.get());
    assertSame(component, captured.get().getComponent());
  }
}
