package com.webforj.component.fontchooser.sink;

import com.basis.bbj.proxies.event.BBjFileChooserApproveEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.fontchooser.FontChooser;
import com.webforj.component.fontchooser.event.FontChooserApproveEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

/**
 * Forwards the approve events of a {@link FontChooser} to the registered listeners.
 *
 * @author Lorenz
 * @since 0.008
 */
public final class FontChooserApproveEventSink {

  private ArrayList<Consumer<FontChooserApproveEvent>> targets;

  private final FontChooser fontChooser;

  /**
   * Creates a new sink for the given font chooser.
   *
   * @param fc the font chooser
   * @param callback the first listener
   */
  @SuppressWarnings({"static-access"})
  public FontChooserApproveEventSink(FontChooser fc, Consumer<FontChooserApproveEvent> callback) {
    this.targets.add(callback);
    this.fontChooser = fc;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(fc);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_FILECHOOSER_APPROVE, this,
          "changeEvent");
    } catch (Exception e) {
      // Environment.logError(e);;
    }

  }

  /**
   * Notifies the registered listeners of an event.
   *
   * @param ev the event
   */
  public void changeEvent(BBjFileChooserApproveEvent ev) { // NOSONAR
    FontChooserApproveEvent dwcEv = new FontChooserApproveEvent(this.fontChooser);
    Iterator<Consumer<FontChooserApproveEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<FontChooserApproveEvent> callback) {
    targets.add(callback);
  }
}
