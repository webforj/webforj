package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class Label extends AbstractDwcControl implements ITextAlignable {

    private BBjStaticText bbjStaticText;

    private Boolean lineWrap = true;

    /* top, right, bottom, left margins,
     * used simple array as user will not 
     * have to interact directly with this
     */
    private Integer[] margins = {0,0,0,0};

    public Label() {
        this("");
    }

    public Label(String text) {
        setText(text);
        this.textAlignment = Alignment.LEFT;
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addStaticText(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText());
            bbjStaticText = (BBjStaticText) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public int getBottomMargin(){
        if(this.ctrl != null){
            try{
                return bbjStaticText.getBottomMargin();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this.margins[2];
    }

    public int getLeftMargin(){
        if(this.ctrl != null){
            try{
                return bbjStaticText.getLeftMargin();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this.margins[3];
    }

    public boolean isLineWrap(){
        if(this.ctrl != null){
            try{
                return bbjStaticText.getLineWrap();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this.lineWrap;
    }

    public int getRightMargin(){
        if(this.ctrl != null){
            try{
                return bbjStaticText.getRightMargin();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this.margins[1];
    }

    public int getTopMargin(){
        if(this.ctrl != null){
            try{
                return bbjStaticText.getTopMargin();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this.margins[0];
    }

    public Label setBottomMargin(int margin){
        if(this.ctrl != null){
            try{
                bbjStaticText.setBottomMargin(margin);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.margins[2] = margin;
        return this;
    }
    
    public Label setLeftMargin(int margin){
        if(this.ctrl != null){
            try{
                bbjStaticText.setLeftMargin(margin);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.margins[3] = margin;
        return this;
    }

    public Label setLineWrap(boolean wrap){
        if(this.ctrl != null){
            try{
                bbjStaticText.setLineWrap(wrap);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.lineWrap = wrap;
        return this;
    }

    public Label setRightMargin(int margin){
        if(this.ctrl != null){
            try{
                bbjStaticText.setRightMargin(margin);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.margins[1] = margin;
        return this;
    }

    public Label setTopMargin(int margin){
        if(this.ctrl != null){
            try{
                bbjStaticText.setTopMargin(margin);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.margins[0] = margin;
        return this;
    }



    @Override
    public Alignment getTextAlignment(){
        return this.textAlignment;
    }

    @Override
    public Label setTextAlignment(Alignment alignment){
        if(this.ctrl != null){
            try{
                ((BBjStaticText) ctrl).setAlignment(alignment.textPosition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        this.textAlignment = alignment;
        return this;
    }



    public Label setText(String text) {
        super.setControlText(text);
        return this;
    }

    public Label setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public Label setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public Label setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public Label setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public Label setID(String id){
        super.setControlID(id);
        return this;
    }

    public Label setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public Label addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public Label removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}
