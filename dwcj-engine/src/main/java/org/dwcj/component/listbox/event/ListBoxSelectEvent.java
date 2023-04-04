package org.dwcj.component.listbox.event;

import org.dwcj.component.ComponentEvent;
import org.dwcj.component.listbox.ListBox;

import java.util.ArrayList;
import java.util.List;

public final class ListBoxSelectEvent implements ComponentEvent {

  private final ListBox control;

  private ArrayList<Object> keys = new ArrayList<>(); // This was uninitialized, added
                                                      // initialization -MH

  public ListBoxSelectEvent(ListBox clistBox) {
    this.control = clistBox;
    this.keys.add(control.getSelectedItem().getKey());
  }

  public void addKey(Object key) {
    keys.add(key);
  }

  public List<Object> getKeys() {
    return keys;
  }

  @Override
  public ListBox getControl() {
    return control;
  }
}
