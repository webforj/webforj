package org.dwcj.templates.pages;

import org.dwcj.controls.label.Label;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.panels.Div;

public class TestDivTwo extends Div {
    
    @Override
    protected void create(AbstractDwcjPanel p) {
        super.create(p);
        this.add(new Label("<html><h1>Div 2</h1></html>"));
        this.add(new Label("<html><p>There's going to be a Counter here sometime!</p></html>"));
    }
    
}
