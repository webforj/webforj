package org.dwcj.templates.pages;

import org.dwcj.controls.label.Label;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.panels.Div;

public class TestDivOne extends Div {
    
    @Override
    protected void create(AbstractDwcjPanel p) {
        super.create(p);
        this.add(new Label("<html><h1>Div One</h1></html>"));
        this.add(new Label("<html><p>There's going to be a Div One here sometime!</p></html>"));
    }
    
}
