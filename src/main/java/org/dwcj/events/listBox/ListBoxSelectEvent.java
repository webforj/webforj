package org.dwcj.events.listBox;

import org.dwcj.controls.ListBox;
import org.dwcj.events.IDwcEvent;

import java.util.ArrayList;
import java.util.Map;

public final class ListBoxSelectEvent implements IDwcEvent {

    private final ListBox control;

    private ArrayList<Object> keys;

    public ListBoxSelectEvent(ListBox clistBox) {
        this.control = clistBox;
        this.keys.add(control.getSelectedItem().getKey());
    }

    public void addKey(Object key) { keys.add(key); }
    public ArrayList<Object> getKeys() { return keys; }

    @Override
    public ListBox getControl() { return control; }
}
