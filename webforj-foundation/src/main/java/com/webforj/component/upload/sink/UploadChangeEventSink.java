package com.webforj.component.upload.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjFileChooserChangeEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.upload.Upload;
import com.webforj.component.upload.event.UploadChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;
import java.util.Map;

/**
 * Maps {@link BBjFileChooserChangeEvent} to {@link UploadChangeEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class UploadChangeEventSink extends AbstractDwcEventSink {

  /**
   * Creates a new sink for the given component and dispatcher.
   *
   * @param component the component
   * @param dispatcher the events dispatcher
   */
  public UploadChangeEventSink(Upload component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_FILECHOOSER_CHANGE);
  }

  /**
   * Reads the selected files vector from the BBj event and dispatches a {@link UploadChangeEvent}.
   *
   * @param ev the BBj event
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjFileChooserChangeEvent change = (BBjFileChooserChangeEvent) ev;
    Map<String, Object> map = new HashMap<>();
    map.put("files", change.getSelectedFiles());

    getEventDispatcher().dispatchEvent(new UploadChangeEvent((Upload) getComponent(), map));
  }
}
