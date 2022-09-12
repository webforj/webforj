package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;

public final class RadioButtonGroup extends AbstractDwcControl {

    private static ArrayList<RadioButtonGroup> radioButtonGroups;

    private BBjRadioGroup radioGroup;

    private ArrayList<RadioButton> radioButtons;

    @Override
    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = (BBjControl) w.addRadioGroup();
            radioGroup = (BBjRadioGroup) ctrl;
            radioButtons = new ArrayList<>();
            catchUp();
            radioButtonGroups.add(this);
        } catch(Exception e) {
            e.printStackTrace();
        }
    }

    public void add(RadioButton radioButton) {
        try {
            radioButtons.add(radioButton);
            radioGroup.add((BBjRadioButton) radioButton.getControl());
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public RadioButton getSelected() {
        try {
            return getButtonByID(radioGroup.getSelected().getID());
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public int getID() {
        BBjControl control = (BBjControl) radioGroup;
        try {
            return control.getID();
        } catch (BBjException e) {
            e.printStackTrace();
            return -1;
        }
    }

    private RadioButton getButtonByID(int id) {
        for (RadioButton radioButton : radioButtons) {
            if (radioButton.getID() == id) {
                return radioButton;
            }
        }
        return null;
    }

    public static RadioButtonGroup getGroupByID(int id) {
        for (RadioButtonGroup radioButtonGroup : radioButtonGroups) {
            if (radioButtonGroup.getID() == id) {
                return radioButtonGroup;
            }
        }
        return null;
    }

    public void remove(RadioButton radioButton) {
        try {
            radioGroup.remove((BBjRadioButton) radioButton.getControl());
            radioButtons.remove(radioButton);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }
}
