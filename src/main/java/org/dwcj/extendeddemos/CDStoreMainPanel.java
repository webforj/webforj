package org.dwcj.extendeddemos;

import com.basiscomponents.db.DataRow;
import com.basiscomponents.db.ResultSet;
import org.dwcj.App;
import org.dwcj.bbjplugins.gridExWidget.GridExWidget;
import org.dwcj.controls.button.Button;
import org.dwcj.controls.label.Label;
import org.dwcj.bbjplugins.gridExWidget.events.GridExWidgetSelectEvent;
import org.dwcj.controls.panels.events.DivClickEvent;
import org.dwcj.controls.stringeditbox.StringEditBox;
import org.dwcj.controls.panels.AppPanel;
import org.dwcj.controls.panels.Div;

public class CDStoreMainPanel extends AppPanel {

    private final Div gridpanel;
    private final Div formpanel;
    private final StringEditBox fieldCdNumber;
    private final StringEditBox fieldTitle;
    private final StringEditBox fieldArtist;
    private final StringEditBox fieldLabel;
    public CDStoreBC bc;
    public CDStoreMainPanel() throws Exception {
        super();

        setVisible(false);
        setStyle("padding", "10px");
        bc = new CDStoreBC();

        Label headline = new Label("<html><h2>CD Store Inventory File</h2>");
        add(headline);

        Div mainpanel = new Div();
        add(mainpanel);
        mainpanel.setStyle("height", "calc( 100vh - 100px )");
        mainpanel.setStyle("width", "100%");


        gridpanel = new Div();
        mainpanel.add(gridpanel);
        formpanel = new Div();
        mainpanel.add(formpanel);

        gridpanel.setStyle("float","left");
        gridpanel.setStyle("width", "100%");
        formpanel.setStyle("width", "0px");
        formpanel.setStyle("float","right");

        gridpanel.setStyle("height", "100%");
        formpanel.setStyle("height", "100%");
        formpanel.setStyle("border", "solid 1px var(--bbj-color-gray-65)");
        formpanel.setStyle("display","grid");
        formpanel.setStyle("gap","10px");
        formpanel.setStyle("padding","40px 10px 10px 10px");

        Div formCloseButtonDiv = new Div();
        formpanel.add(formCloseButtonDiv);
        Label formCloseButton = new Label();
        formCloseButton.setText("<html><bbj-icon pool='tabler' theme='gray' name='square-x'></bbj-icon></html>");
        formCloseButtonDiv.setStyle("position","absolute");
        formCloseButtonDiv.setStyle("top","5px");
        formCloseButtonDiv.setStyle("right","5px");
        formCloseButtonDiv.add(formCloseButton);
        formCloseButtonDiv.onClick(this::onFormCloseClick);



        fieldCdNumber = new StringEditBox();
        fieldCdNumber.setMask("000000")
                     .setAttribute("label","CD Number");
        formpanel.add(fieldCdNumber);

        fieldTitle = new StringEditBox();
        fieldTitle.setAttribute("label","Title");
        formpanel.add(fieldTitle);

        fieldArtist = new StringEditBox();
        fieldArtist.setAttribute("label","Title");
        formpanel.add(fieldArtist);

        fieldLabel = new StringEditBox();
        fieldLabel.setAttribute("label","Title");
        formpanel.add(fieldLabel);

        GridExWidget grid = new GridExWidget();
        gridpanel.add(grid);

        grid.setStyle("height", "calc( 100vh - 100px )");
        grid.setStyle("width", "100%");

        ResultSet rs = bc.retrieve();
        grid.setData(rs);

        grid.onSelect(this::onGridSelect);

        Label footer = new Label("Some Footer");
        add(footer);

        setVisible(true);
        App.busy(false);
    }

    private void onFormCloseClick(DivClickEvent divClickEvent) {
        closePanel();
    }


    private void onGridSelect(GridExWidgetSelectEvent BBjGridExWidgetSelectEvent) {
        ResultSet sel = BBjGridExWidgetSelectEvent.getSelection();
        if (sel.size()==1) {
            DataRow rec = sel.get(0);
            openPanel();
            populateForm(rec);
        }
    }

    private void populateForm(DataRow rec) {
        fieldCdNumber.setText(rec.getFieldAsString("CDNUMBER"));
        fieldTitle.setText(rec.getFieldAsString("TITLE"));
        fieldArtist.setText(rec.getFieldAsString("ARTIST"));
        fieldLabel.setText(rec.getFieldAsString("LABEL"));

    }

    private void openPanel() {
        formpanel.setStyle("width","200px");
        gridpanel.setStyle("width","calc( 100% - 200px )");
    }

    private void closePanel() {
        formpanel.setStyle("width","0px");
        gridpanel.setStyle("width","100%");
    }

}
