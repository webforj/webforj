package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class Label extends AbstractDwcControl {

    private BBjStaticText bbjStaticText;

    public Label() {
    }

    public Label(String text) {
        setText(text);
    }

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addStaticText(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText());
            catchUp();
            bbjStaticText = (BBjStaticText) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public int getBottomMargin(){
        try{
            return bbjStaticText.getBottomMargin();
        } catch (BBjException e){
            e.printStackTrace();
        }
        return -1;
    }

    public int getLeftMargin(){
        try{
            return bbjStaticText.getLeftMargin();
        } catch (BBjException e){
            e.printStackTrace();
        }
        return -1;
    }

    public boolean isLineWrap(){
        try{
            return bbjStaticText.getLineWrap();
        } catch (BBjException e){
            e.printStackTrace();
        }
        return true; //default is line wrap
    }

    public int getRightMargin(){
        try{
            return bbjStaticText.getRightMargin();
        } catch (BBjException e){
            e.printStackTrace();
        }
        return -1;
    }

    public int getTopMargin(){
        try{
            return bbjStaticText.getTopMargin();
        } catch (BBjException e){
            e.printStackTrace();
        }
        return -1;
    }

    public Label setBottomMargin(int margin){
        try{
            bbjStaticText.setBottomMargin(margin);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this;
    }
    
    public Label setLeftMargin(int margin){
        try{
            bbjStaticText.setLeftMargin(margin);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    public Label setLineWrap(boolean wrap){
        try{
            bbjStaticText.setLineWrap(wrap);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    public Label setRightMargin(int margin){
        try{
            bbjStaticText.setRightMargin(margin);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    public Label setTopMargin(int margin){
        try{
            bbjStaticText.setTopMargin(margin);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public Label setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public Label addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public Label removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public Label setID(String id){
        super.setID(id);
        return this;
    }
}
