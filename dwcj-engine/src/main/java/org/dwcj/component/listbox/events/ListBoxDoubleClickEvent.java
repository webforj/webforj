package org.dwcj.component.listbox.events;

import org.dwcj.component.listbox.ListBox;
import org.dwcj.interfaces.ControlEvent;

import java.util.ArrayList;
import java.util.List;


public class ListBoxDoubleClickEvent implements ControlEvent{
    
    private final ListBox control;

    private ArrayList<Object> keys = new ArrayList<>(); 


    public ListBoxDoubleClickEvent(ListBox clistBox) {
        this.control = clistBox;
        this.keys.add(control.getSelectedItem().getKey());
    }

    public void addKey(Object key) { keys.add(key); }
    public List<Object> getKeys() { return keys; }


    @Override
    public ListBox getControl() { return control; }
}
