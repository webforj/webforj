package org.dwcj.component.tabbedpane.sink;


import com.basis.bbj.proxies.event.BBjTabSelectedEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.tabbedpane.TabbedPane;
import org.dwcj.component.tabbedpane.event.TabSelectEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class TabSelectEventSink {

  private ArrayList<Consumer<TabSelectEvent>> targets;
  private final TabbedPane tabControl;

  @SuppressWarnings({"static-access"})
  public TabSelectEventSink(TabbedPane btn) {

    this.targets = new ArrayList<>();
    this.tabControl = btn;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(btn);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_TAB_SELECT, // NOSONAR
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "pushEvent"), "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public TabSelectEventSink(TabbedPane tabControl, Consumer<TabSelectEvent> callback) {

    this.targets = new ArrayList<>();
    this.targets.add(callback);
    this.tabControl = tabControl;

    BBjControl bbjctrl = null;
    try {
      bbjctrl = ComponentAccessor.getDefault().getBBjControl(tabControl);
      bbjctrl.setCallback(Environment.getCurrent().getBBjAPI().ON_BUTTON_PUSH, // NOSONAR
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "pushEvent"), "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public void pushEvent(BBjTabSelectedEvent ev) { // NOSONAR
    TabSelectEvent dwcEv = null;
    try {
      dwcEv = new TabSelectEvent(this.tabControl, ev.getIndex(), ev.getTitle());
    } catch (BBjException e) {
      Environment.logError(e);
    }
    Iterator<Consumer<TabSelectEvent>> it = targets.iterator();
    while (it.hasNext())
      it.next().accept(dwcEv);
  }


  public void addCallback(Consumer<TabSelectEvent> callback) {
    targets.add(callback);
  }
}
