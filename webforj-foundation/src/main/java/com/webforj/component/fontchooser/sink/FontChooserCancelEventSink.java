package com.webforj.component.fontchooser.sink;

import com.basis.bbj.proxies.event.BBjFileChooserCancelEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.fontchooser.FontChooser;
import com.webforj.component.fontchooser.event.FontChooserCancelEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class FontChooserCancelEventSink {

  private ArrayList<Consumer<FontChooserCancelEvent>> targets;

  private final FontChooser fontChooser;

  @SuppressWarnings({"static-access"})
  public FontChooserCancelEventSink(FontChooser fc, Consumer<FontChooserCancelEvent> callback) {
    this.targets.add(callback);
    this.fontChooser = fc;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(fc);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_FILECHOOSER_CANCEL,
          Environment.getCurrent().getBridge().getEventProxy(this, "cancelEvent"), "onEvent");
    } catch (Exception e) {
      // Environment.logError(e);;
    }

  }

  public void changeEvent(BBjFileChooserCancelEvent ev) { // NOSONAR
    FontChooserCancelEvent dwcEv = new FontChooserCancelEvent(this.fontChooser);
    Iterator<Consumer<FontChooserCancelEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<FontChooserCancelEvent> callback) {
    targets.add(callback);
  }
}
