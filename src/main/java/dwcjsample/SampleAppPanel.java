package dwcjsample;

import org.dwcj.App;
import org.dwcj.controls.Button;
import org.dwcj.controls.ComboBox;
import org.dwcj.controls.IExpansible.Expanse;
import org.dwcj.controls.IThemable.Theme;
import org.dwcj.controls.Label;
import org.dwcj.controls.TextBox;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.events.PageLoadedEvent;
import org.dwcj.events.ValueChangedEvent;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.panels.AppPanel;
import org.dwcj.panels.Div;
import org.dwcj.shoelacecontrols.Rating;

import java.util.HashMap;
import java.util.Map;

public class SampleAppPanel extends AppPanel {

    private final TextBox ed_firstname;
    private final TextBox ed_lastname;
    private final ComboBox cb_gender;
    private final Label ratingText;
    private final Rating ratingctrl;

    public SampleAppPanel() throws DwcAppInitializeException {

        super();

        setStyle("display", "inline-grid");
        setStyle("grid-template-columns", "1fr 2fr");
        setStyle("gap", "20px");
        setStyle("left", "20px");
        setStyle("top", "20px");
        setStyle("border", "1px dotted");
        setStyle("padding", "10px");

        add(new Label("Firstname:"));
        ed_firstname = new TextBox();
        add(ed_firstname);

        add(new Label("Lastname:"));
        ed_lastname = new TextBox("");
        add(ed_lastname);

        add(new Label("Gender:"));
        Map<Object, String> genders = new HashMap<>();
        genders.put("m", "Male");
        genders.put("f", "Female");
        genders.put("o", "Other");
        cb_gender = new ComboBox();
        cb_gender.setItems(genders);
        add(cb_gender);
        cb_gender.setStyle("width", "100%");

        add(new Label("Rating:"));

        ratingctrl = new Rating();
        add(ratingctrl);
        ratingctrl.onValueChanged(this::onRatingChanged);

        add(new Label(""));
        ratingText = new Label("");
        add(ratingText);
        Button btn = new Button("Say Hello");
        add(btn);

        Div d = new Div();
        add(d);
        d.setStyle("height","100px");
        d.setStyle("width","100px");
        d.setStyle("background","yellow");

        btn.setTheme(Theme.WARNING);

        btn.setExpanse(Expanse.XLARGE);

        btn.setStyle("grid-column", "1 / span 2");
        btn.setStyle("width", "100%");

        btn.onClick(this::onSampleButtonPush);

    }

    private void onRatingChanged(ValueChangedEvent valueChangedEvent) {

        Double v = valueChangedEvent.getValue();
        String txt = "Poor";
        if (v > 0.0) txt = "Naja";
        if (v > 1.0) txt = "Hmm";
        if (v > 2.0) txt = "Just about";
        if (v > 3.0) txt = "Acceptable";
        if (v > 4.0) txt = "Supergeil";

        ratingText.setText(txt);
    }

    private void onPageLoaded(PageLoadedEvent pageLoadedEvent) {
        App.msgbox("loaded");
    }

    private void onSampleButtonPush(ButtonPushEvent ev) {
        String text = ed_firstname.getText() + " " + ed_lastname.getText() + " (" + cb_gender.getText() + ")";
        App.msgbox(text, 0, "Hello World");

        ratingctrl.setMaxValue(ratingctrl.getMaxValue() + 1);
        ratingctrl.setValue(ratingctrl.getValue() + 1);
        ratingctrl.setPrecision(ratingctrl.getPrecision() / 2);


    }


}
