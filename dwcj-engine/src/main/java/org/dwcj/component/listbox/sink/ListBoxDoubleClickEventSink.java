package org.dwcj.component.listbox.sink;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.listbox.ListBox;
import org.dwcj.component.listbox.event.ListBoxDoubleClickEvent;

import com.basis.bbj.proxies.event.BBjListDoubleClickEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;

public class ListBoxDoubleClickEventSink {

  private final ArrayList<Consumer<ListBoxDoubleClickEvent>> targets;
  private final ListBox listBox;

  @SuppressWarnings({"static-access"})
  public ListBoxDoubleClickEventSink(ListBox listBox) {
    this.targets = new ArrayList<>();
    this.listBox = listBox;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(listBox);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_LIST_DOUBLE_CLICK,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "doubleClickEvent"),
          "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings({"static-access"})
  public ListBoxDoubleClickEventSink(ListBox listBox, Consumer<ListBoxDoubleClickEvent> callback) {
    this.targets = new ArrayList<>();
    this.targets.add(callback);
    this.listBox = listBox;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(listBox);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_LIST_DOUBLE_CLICK,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "doubleClickEvent"),
          "onEvent");
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings("java:S1172")
  public void doubleClickEvent(BBjListDoubleClickEvent ev) { // NOSONAR
    ListBoxDoubleClickEvent dwcEv = new ListBoxDoubleClickEvent(this.listBox);
    Iterator<Consumer<ListBoxDoubleClickEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void doDoubleClick(Object key) {
    ListBoxDoubleClickEvent dwcEv = new ListBoxDoubleClickEvent(listBox);
    dwcEv.addKey(key);
    Iterator<Consumer<ListBoxDoubleClickEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }

  public void addCallback(Consumer<ListBoxDoubleClickEvent> callback) {
    targets.add(callback);
  }

}
