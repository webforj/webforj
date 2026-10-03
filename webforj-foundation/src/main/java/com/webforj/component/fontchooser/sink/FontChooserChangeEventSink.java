package com.webforj.component.fontchooser.sink;

import com.basis.bbj.proxies.event.BBjFileChooserChangeEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.fontchooser.FontChooser;
import com.webforj.component.fontchooser.event.FontChooserChangeEvent;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

/**
 * Forwards the change events of a {@link FontChooser} to the registered listeners.
 *
 * @author Lorenz
 * @since 0.008
 */
public final class FontChooserChangeEventSink {

  private ArrayList<Consumer<FontChooserChangeEvent>> targets;

  private final FontChooser fontChooser;

  /**
   * Creates a new sink for the given font chooser.
   *
   * @param fc the font chooser
   * @param callback the first listener
   */
  @SuppressWarnings({"static-access"})
  public FontChooserChangeEventSink(FontChooser fc, Consumer<FontChooserChangeEvent> callback) {
    this.targets.add(callback);
    this.fontChooser = fc;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(fc);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_FILECHOOSER_CHANGE, this,
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
  public void changeEvent(BBjFileChooserChangeEvent ev) { // NOSONAR
    FontChooserChangeEvent dwcEv = new FontChooserChangeEvent(this.fontChooser);
    Iterator<Consumer<FontChooserChangeEvent>> it = targets.iterator();
    while (it.hasNext()) {
      it.next().accept(dwcEv);
    }
  }

  public void addCallback(Consumer<FontChooserChangeEvent> callback) {
    targets.add(callback);
  }
}
