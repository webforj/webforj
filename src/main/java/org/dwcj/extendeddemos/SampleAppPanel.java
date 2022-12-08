package org.dwcj.extendeddemos;

import org.dwcj.App;
import org.dwcj.controls.button.Button;
import org.dwcj.controls.button.events.ButtonPushEvent;
import org.dwcj.controls.combobox.ComboBox;
import org.dwcj.controls.label.Label;
import org.dwcj.controls.panels.AppPanel;
import org.dwcj.controls.textbox.TextBox;
import org.dwcj.exceptions.DwcAppInitializeException;


import java.util.HashMap;
import java.util.Map;

public class SampleAppPanel extends AppPanel {

    private final TextBox edFirstname;
    private final TextBox edLastname;
    private final ComboBox cbGender;
    private final Label ratingText;

    public SampleAppPanel() throws DwcAppInitializeException {

        super();

        //setting some styles of the app panel itself
        setStyle("display", "inline-grid");
        setStyle("grid-template-columns", "1fr 2fr");
        setStyle("gap", "20px");
        setStyle("left", "20px");
        setStyle("top", "20px");
        setStyle("border", "1px dotted");
        setStyle("padding", "10px");

        // add a few labels and data fields

        add(new Label("Firstname:"));
        edFirstname = new TextBox();
        add(edFirstname);

        add(new Label("Lastname:"));
        edLastname = new TextBox("");
        add(edLastname);

        add(new Label("Gender:"));
        Map<Object, String> genders = new HashMap<>();
        genders.put("m", "Male");
        genders.put("f", "Female");
        genders.put("o", "Other");
        cbGender = new ComboBox();
        cbGender.setItems(genders);
        add(cbGender);
        cbGender.setStyle("width", "100%");

        // add(new Label("Rating:"));

        //the rating control from shoelace

        // ratingctrl = new Rating();
        // add(ratingctrl);
        // ratingctrl.onValueChanged(this::onRatingChanged);

        add(new Label(""));
        ratingText = new Label("");
        add(ratingText);

        //the submit button
        Button btn = new Button("Say Hello");
        add(btn);
        btn.setTheme(Button.Theme.WARNING);
        btn.setExpanse(Button.Expanse.XLARGE);
        btn.setStyle("grid-column", "1 / span 2");
        btn.setStyle("width", "100%");
        btn.onClick(this::onSampleButtonPush);

    }

    // private void onRatingChanged(RatingValueChangedEvent ratingValueChangedEvent) {

    //     Double v = ratingValueChangedEvent.getValue();
    //     String txt = "Poor";
    //     if (v > 0.0) txt = "Naja";
    //     if (v > 1.0) txt = "Hmm";
    //     if (v > 2.0) txt = "Just about";
    //     if (v > 3.0) txt = "Acceptable";
    //     if (v > 4.0) txt = "Supergeil";

    //     ratingText.setText(txt);
    // }

    private void onSampleButtonPush(ButtonPushEvent ev) {
        String text = edFirstname.getText() + " " + edLastname.getText() + " (" + cbGender.getText() + ")";
        App.msgbox(text, 0, "Hello World");
    }

}
