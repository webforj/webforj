package com.webforj.component.upload.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjFileChooserFilterEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.upload.Upload;
import com.webforj.component.upload.event.UploadFilterChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;
import java.util.Map;

/**
 * Maps {@link BBjFileChooserFilterEvent} to {@link UploadFilterChangeEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class UploadFilterChangeEventSink extends AbstractDwcEventSink {

  /**
   * Creates a new sink for the given component and dispatcher.
   *
   * @param component the component
   * @param dispatcher the events dispatcher
   */
  public UploadFilterChangeEventSink(Upload component, EventDispatcher dispatcher) {
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

    UploadFilterChangeEvent dwcEv = new UploadFilterChangeEvent((Upload) getComponent(), map);
    getEventDispatcher().dispatchEvent(dwcEv);
  }
}
