package com.webforj.component.fileupload.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjFileChooserChangeEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.fileupload.FileUpload;
import com.webforj.component.fileupload.event.FileUploadChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;
import java.util.Map;

/**
 * Maps {@link BBjFileChooserChangeEvent} to {@link FileUploadChangeEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class FileUploadChangeEventSink extends AbstractDwcEventSink {

  /**
   * Creates a new sink for the given component and dispatcher.
   *
   * @param component the component
   * @param dispatcher the events dispatcher
   */
  public FileUploadChangeEventSink(FileUpload component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_FILECHOOSER_CHANGE);
  }

  /**
   * Reads the selected files vector from the BBj event and dispatches a
   * {@link FileUploadChangeEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjFileChooserChangeEvent change = (BBjFileChooserChangeEvent) ev;
    Map<String, Object> map = new HashMap<>();
    map.put("files", change.getSelectedFiles());

    getEventDispatcher().dispatchEvent(new FileUploadChangeEvent((FileUpload) getComponent(), map));
  }
}
