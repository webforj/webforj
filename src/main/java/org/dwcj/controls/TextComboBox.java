package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.Iterator;
import java.util.Map;

/**
 * ComboBoxEdit Control
 */
public final class TextComboBox extends AbstractDwclistControl implements IReadOnly, IFocusable, IMouseWheelEnableable, ITabTraversable, ITextAlignable {

    private BBjListEdit bbjListEdit;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, PRIMARY, SUCCESS, WARNING
    }

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addListEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            populate();
            catchUp();
            this.bbjListEdit = (BBjListEdit) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // @SuppressWarnings("unchecked")
    // protected void populate() {
    //     if (values != null && ctrl != null) try {
    //         BBjListEdit cb = (BBjListEdit) ctrl;
    //         cb.removeAllItems();
    //         BBjVector v = new BBjVector();
    //         Iterator<Object> it = values.keySet().iterator();
    //         while (it.hasNext()) {
    //             v.add(values.get(it.next()));
    //         }
    //         cb.insertItems(0, v);
    //     } catch (BBjException e) {
    //         e.printStackTrace();
    //     }

    // }

    // public TextComboBox addItem(Object key, String item) {
    //     this.values.put(key, item);
    //     populate();
    //     return this;
    // }

    // public TextComboBox insertItemAt(Object key, String item, Integer index){
    //     this.values.put(key, item);
    //     if(this.ctrl != null){
    //         try{
    //             BBjListEdit cb = (BBjListEdit) ctrl;
    //             BBjVector v = new BBjVector();
    //             v.add(values.get(key));
    //             cb.insertItems(index, v);
    //         } catch(BBjException e){
    //             e.printStackTrace();
    //         }
    //     }
    //     return this;
    // }

    // public TextComboBox addItems(Map<Object, String> items){
    //     this.values.putAll(items);
    //     this.populate();
    //     return this;
    // }

    // public TextComboBox insertItemsAt(Map<Object, String> items, Integer index){
    //     this.values.putAll(items);
    //     if(this.ctrl != null){
    //         try{
    //             BBjListEdit cb = (BBjListEdit) ctrl;
    //             Iterator<Object> it = items.keySet().iterator();
    //             Integer counter = 0; 
    //             while (it.hasNext()) {
    //                 BBjVector v = new BBjVector();
    //                 v.add(values.get(it.next()));
    //                 cb.insertItems(index + counter++, v);
    //             }
    //         } catch(BBjException e){
    //             e.printStackTrace();
    //         }
    //     }
    //     return this;
    // }


    public TextComboBox addItem(Object key, String item) {
        this.values.put(key, item);
        data2.add(values.get(key));
        populate();
        return this;
    }

    public TextComboBox insertItemAt(Object key, String item, Integer index){
        this.values.put(key, item);
        data2.add(index, values.get(key));
        populate();
        return this;
    }

    public TextComboBox addItems(Map<Object, String> items){
        this.values.putAll(items);
        Iterator<Object> it = items.keySet().iterator();
        while (it.hasNext()) {
            data2.add(values.get(it.next()));
        }
        populate();
        return this;
    }

    public TextComboBox insertItemsAt(Map<Object, String> items, Integer index){
        this.values.putAll(items);
        Iterator<Object> it = items.keySet().iterator();
        Integer counter = 0;
        while (it.hasNext()) {
            data2.add(index + counter++, values.get(it.next()));
        }
        populate();
        return this;
    }

    @SuppressWarnings("unchecked")
    protected void populate() {
        if(this.ctrl != null){
            try{
                BBjListEdit cb = (BBjListEdit) ctrl;
                cb.removeAllItems();
                cb.insertItems(0, data2);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
    }







    public void closeList() {
        try {
            bbjListEdit.closeList();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void deselect() {
        try {
            bbjListEdit.deselect();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public Map<Object, String> getAllItems() {
            return values;
    }

    public String getEditText() {
        try {
            return bbjListEdit.getEditText();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getItemAt(Object key) {
        return values.get(key);
    }

    public int getItemCount() {
        try {
            return bbjListEdit.getItemCount();
        } catch (BBjException e) {
            e.printStackTrace();
            return -1;
        }
    }

    public int getSelectedIndex() {
        try {
            return bbjListEdit.getSelectedIndex();
        } catch (BBjException e) {
            e.printStackTrace();
            return -1;
        }
    }


    public void openList() {
        try {
            bbjListEdit.openList();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeAllItems() {
        try {
            bbjListEdit.removeAllItems();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }


    public void setEditText(String text) {
        try {
            bbjListEdit.setEditText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    /**
     * set the list of items into the ComboBoxEdit
     *
     * @param values A Map object containing the key-value pairs for the list
     * @return the control itself
     */
    public TextComboBox setItems(Map<Object, String> values) {
        this.values = values;
        Iterator<Object> it = values.keySet().iterator();
        while (it.hasNext()) {
            data2.add(values.get(it.next()));
        }
        populate();
        return this;
    }

    public void setMaximumRowCount(int max) {
        try {
            bbjListEdit.setMaximumRowCount(max);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }



    

    @Override
    public Boolean isReadOnly(){
        if(this.ctrl != null){
            try{
                return ((BBjListEdit) this.ctrl).isEditable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public TextComboBox setReadOnly(Boolean readOnly){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setEditable(readOnly);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                return ((BBjListEdit) this.ctrl).isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public TextComboBox setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setFocusable(focusable);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public MouseWheelCondition getScrollWheelBehavior(){
        if(this.ctrl != null){
            return this.mouseWheelCondition;
        }
        return null;
    }

    @Override
    public TextComboBox setScrollWheelBehavior(MouseWheelCondition condition){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;

    }

    @Override
    public Boolean isTabTraversable(){
        if(this.ctrl != null){
            try{
                return ((BBjListEdit) this.ctrl).isTabTraversable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override 
    public TextComboBox setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setTabTraversable(traversable);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public Alignment getTextAlignment(){
        if(this.ctrl != null){
            return this.textAlignment;
        }
        return null;
    }

    @Override
    public TextComboBox setTextAlignment(Alignment textAlignment){
        if(this.ctrl != null){
            try{
                ((BBjListEdit) this.ctrl).setAlignment(textAlignment.textPosition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }




    @Override
    public TextComboBox setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public TextComboBox setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public TextComboBox setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public TextComboBox setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public TextComboBox setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public TextComboBox setID(String id){
        super.setControlID(id);
        return this;
    }

    @Override
    public TextComboBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    @Override
    public TextComboBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public TextComboBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }


    

    public TextComboBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public TextComboBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

}
