package org.dwcj.shoelacecontrols;

import com.basis.bbj.proxies.sysgui.BBjHtmlView;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.IControl;
import org.dwcj.controls.IStyleable;
import org.dwcj.panels.AbstractDwcjPanel;
import org.dwcj.panels.Div;

public final class SplitPanel extends AbstractShoelaceControl implements IStyleable {

    final private String uuid = "id" + java.util.UUID.randomUUID().toString().replace("-", "");

    private BBjHtmlView htmlv;
    private Div startPanel;
    private Div endPanel;
    private Boolean vertical = false;
    private int position = 50;


    public SplitPanel() {
    }

    /**
     * create a split panel; the default is horizontal if not specified with this constructor
     *
     * @param fVertical true to create a vertical splitter
     */
    public SplitPanel(Boolean fVertical) {
        this.vertical = fVertical;
    }

    @Override
    protected void create(AbstractDwcjPanel p) {

        String v = vertical ? "" : "vertical";

        final String html = "<sl-split-panel id='" + uuid + "' position='" + position + "' " + v + " style='height:100%; width:100%;'>" + "<sl-icon slot='handle' name='grip-vertical'></sl-icon>" + "</sl-split-panel>";

        try {

            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //create the HtmlView invisible to avoid visible rendering
            byte[] b1 = new byte[]{(byte) 0x00, (byte) 0x10};
            htmlv = w.addHtmlView(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", b1);
            htmlv.setText(html);
            ctrl = htmlv;

            startPanel = new Div();
            ControlAccessor.getDefault().create(startPanel, p);
            startPanel.setAttribute("slot", "start");
            endPanel = new Div();
            ControlAccessor.getDefault().create(endPanel, p);
            endPanel.setAttribute("slot", "end");

            Environment.getInstance().getSysGui().executeScript("document.getElementById('" + uuid + "').appendChild(document.getElementById('" + startPanel.getAttribute("id") + "'));" + "document.getElementById('" + uuid + "').appendChild(document.getElementById('" + endPanel.getAttribute("id") + "'));");

            htmlv.setVisible(true);


        } catch (Exception e) {
            e.printStackTrace();
        }
        loadShoelaceLib();
    }

    @Override
    public SplitPanel setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public SplitPanel addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public SplitPanel removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    /**
     * check if the split control is vertical
     *
     * @return true if vertical, false if horizontal
     */
    public Boolean getVertical() {
        return vertical;
    }

    /**
     * obtain a reference to the start (1st) panel
     * upper if horizontal, left if vertical
     *
     * @return the panel div
     */
    public Div getStartPanel() {
        return startPanel;
    }

    /**
     * obtain a reference to the end (2nd) panel
     * lower if horizontal, right if vertical
     *
     * @return the panel div
     */
    public Div getEndPanel() {
        return endPanel;
    }

    /**
     * set the split position
     *
     * @param position the position in percent
     */
    public void setPosition(int position) {
        this.position = position;
        if (htmlv != null) {
            try {
                Environment.getInstance().getSysGui().executeScript("document.getElementById('" + uuid + "').setAttribute('position'," + position + ");");
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
    }


}
