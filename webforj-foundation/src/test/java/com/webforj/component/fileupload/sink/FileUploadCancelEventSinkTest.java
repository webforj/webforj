package com.webforj.component.fileupload.sink;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.basis.bbj.proxies.event.BBjFileChooserCancelEvent;
import com.webforj.component.fileupload.FileUpload;
import com.webforj.component.fileupload.event.FileUploadCancelEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class FileUploadCancelEventSinkTest {

  @Mock
  BBjFileChooserCancelEvent bbjEvent;

  FileUpload component;
  EventDispatcher dispatcher;
  FileUploadCancelEventSink sink;
  AtomicReference<FileUploadCancelEvent> captured;

  @BeforeEach
  void setUp() {
    component = new FileUpload();
    dispatcher = new EventDispatcher();
    sink = new FileUploadCancelEventSink(component, dispatcher);
    captured = new AtomicReference<>();
    dispatcher.addListener(FileUploadCancelEvent.class, captured::set);
  }

  @Test
  void shouldDispatchTypedEventWithComponentSource() {
    sink.handleEvent(bbjEvent);

    assertNotNull(captured.get());
    assertSame(component, captured.get().getComponent());
  }
}
