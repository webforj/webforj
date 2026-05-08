package com.webforj.component.fileupload.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjFileChooserFilterEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.fileupload.FileUpload;
import com.webforj.component.fileupload.event.FileUploadFilterChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;
import java.util.Map;

/**
 * Maps {@link BBjFileChooserFilterEvent} to {@link FileUploadFilterChangeEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class FileUploadFilterChangeEventSink extends AbstractDwcEventSink {

  /**
   * Creates a new sink for the given component and dispatcher.
   *
   * @param component the component
   * @param dispatcher the events dispatcher
   */
  public FileUploadFilterChangeEventSink(FileUpload component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_FILECHOOSER_FILTER);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjFileChooserFilterEvent event = (BBjFileChooserFilterEvent) ev;
    String filter = event.getActiveFileFilter();
    Map<String, Object> map = new HashMap<>();
    map.put("filter", filter == null ? "" : filter);

    FileUploadFilterChangeEvent dwcEv =
        new FileUploadFilterChangeEvent((FileUpload) getComponent(), map);
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
