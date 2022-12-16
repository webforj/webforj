package org.dwcj.controls.label;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.util.BBjFunctionalityHelper;

public final class Label extends AbstractDwcControl implements TextAlignable {

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
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addStaticText(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText(), flags);
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



    @Override
    public Label setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public Label setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public Label setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public Label setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public Label setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public Label setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public Label setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public Label addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public Label removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }


    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        
        super.catchUp();

        if(Boolean.TRUE.equals(this.lineWrap)){
            this.setLineWrap(this.lineWrap);
        }


        if(this.margins[0] != 0){
            this.setTopMargin(this.margins[0]);
        }
        if(this.margins[1] != 0){
            this.setRightMargin(this.margins[1]);
        }
        if(this.margins[2] != 0){
            this.setBottomMargin(this.margins[2]);
        }
        if(this.margins[3] != 0){
            this.setLeftMargin(this.margins[3]);
        }

    }



}
