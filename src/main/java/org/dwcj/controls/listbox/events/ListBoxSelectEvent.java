package org.dwcj.controls.listbox.events;

import org.dwcj.controls.listbox.ListBox;
import org.dwcj.interfaces.DwcEvent;

import java.util.ArrayList;

public final class ListBoxSelectEvent implements DwcEvent {

    private final ListBox control;

    private ArrayList<Object> keys = new ArrayList<>(); //This was uninitialized, added initialization -MH

    public ListBoxSelectEvent(ListBox clistBox) {
        this.control = clistBox;
        this.keys.add(control.getSelectedItem().getKey());
    }

    public void addKey(Object key) { keys.add(key); }
    public ArrayList<Object> getKeys() { return keys; }

    @Override
    public ListBox getControl() { return control; }
}
