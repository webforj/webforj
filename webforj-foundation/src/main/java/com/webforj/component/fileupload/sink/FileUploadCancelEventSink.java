package com.webforj.component.fileupload.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.fileupload.FileUpload;
import com.webforj.component.fileupload.event.FileUploadCancelEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;
import java.util.Map;

/**
 * Maps the BBj cancel event to {@link FileUploadCancelEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class FileUploadCancelEventSink extends AbstractDwcEventSink {

  /**
   * Creates a new sink for the given component and dispatcher.
   *
   * @param component the component
   * @param dispatcher the events dispatcher
   */
  public FileUploadCancelEventSink(FileUpload component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_FILECHOOSER_CANCEL);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    Map<String, Object> map = new HashMap<>();
    FileUploadCancelEvent dwcEv = new FileUploadCancelEvent((FileUpload) getComponent(), map);
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
