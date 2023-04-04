package org.dwcj.component.combobox.sink;

import com.basis.bbj.proxies.event.BBjListSelectEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.combobox.ComboBox;
import org.dwcj.component.combobox.event.ComboBoxSelectEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class ComboBoxSelectEventSink {

  private ArrayList<Consumer<ComboBoxSelectEvent>> targets = new ArrayList<>();

  private final ComboBox textComboBox;

  private BBjControl bbjctrl;

  @SuppressWarnings({"static-access"})
  public ComboBoxSelectEventSink(ComboBox cb) {
    this.textComboBox = cb;

    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
      bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings({"static-access"})
  public ComboBoxSelectEventSink(ComboBox cb, Consumer<ComboBoxSelectEvent> callback) {
    this.targets.add(callback);
    this.textComboBox = cb;

    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(cb);
      bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_SELECT,
          Environment.getInstance().getDwcjHelper().getEventProxy(this, "selectEvent"), "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings("java.S1172")
  public void selectEvent(BBjListSelectEvent ev) { // NOSONAR
    ComboBoxSelectEvent dwcEv = new ComboBoxSelectEvent(this.textComboBox);
    Iterator<Consumer<ComboBoxSelectEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void doSelect(Object key) {
    ComboBoxSelectEvent dwcEv = new ComboBoxSelectEvent(this.textComboBox);
    dwcEv.setKey(key);
    Iterator<Consumer<ComboBoxSelectEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<ComboBoxSelectEvent> callback) {
    targets.add(callback);
  }
}
