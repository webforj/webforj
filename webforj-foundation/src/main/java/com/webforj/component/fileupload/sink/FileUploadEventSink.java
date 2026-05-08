package com.webforj.component.fileupload.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjFileChooserApproveEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.webforj.component.event.sink.AbstractDwcEventSink;
import com.webforj.component.fileupload.FileUpload;
import com.webforj.component.fileupload.event.FileUploadEvent;
import com.webforj.dispatcher.EventDispatcher;
import java.util.HashMap;
import java.util.Map;

/**
 * Maps {@link BBjFileChooserApproveEvent} to {@link FileUploadEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class FileUploadEventSink extends AbstractDwcEventSink {

  /**
   * Creates a new sink for the given component and dispatcher.
   *
   * @param component the component
   * @param dispatcher the events dispatcher
   */
  public FileUploadEventSink(FileUpload component, EventDispatcher dispatcher) {
    super(component, dispatcher, SysGuiEventConstants.ON_FILECHOOSER_APPROVE);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void handleEvent(BBjEvent ev) {
    BBjFileChooserApproveEvent approve = (BBjFileChooserApproveEvent) ev;
    Map<String, Object> map = new HashMap<>();
    map.put("files", approve.getSelectedFiles());

    getEventDispatcher().dispatchEvent(new FileUploadEvent((FileUpload) getComponent(), map));
  }
}
