package dwcjsample;

import java.util.HashMap;
import java.util.Map;

import org.dwcj.App;
import org.dwcj.controls.Button;
import org.dwcj.controls.ComboBox;
import org.dwcj.controls.TextBox;
import org.dwcj.controls.Label;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.panels.AppPanel;

import com.basis.startup.type.BBjException;

public class SampleAppPanel extends AppPanel {

	private TextBox ed_firstname;
	private TextBox ed_lastname;
	private ComboBox cb_gender;

	public SampleAppPanel() throws DwcAppInitializeException {
		
		super();
		
		setStyle("display","inline-grid");
		setStyle("grid-template-columns","1fr 2fr");
		setStyle("gap","20px");
		setStyle("left","20px");
		setStyle("top","20px");
		setStyle("border","1px dotted");
		setStyle("padding","10px");
		
		add(new Label("Firstname:"));
		ed_firstname = new TextBox();
		add(ed_firstname);
		
		add(new Label("Lastname:"));
		ed_lastname=new TextBox("");
		add(ed_lastname);

		add(new Label("Gender:"));
		Map<Object, String>genders = new HashMap<>();
		genders.put("m","Male");
		genders.put("f","Female");
		genders.put("o","Other");
		cb_gender=new ComboBox();
		cb_gender.setItems(genders);
		add(cb_gender);
		cb_gender.setStyle("width","100%");
		
		Button btn = new Button("Say Hello");
		add(btn);
		
		btn.setTheme(Button.THEME_SUCCESS);
		btn.setExpanse(Button.EXPANSE_XLARGE);

		btn.setStyle("grid-column","1 / span 2");
		btn.setStyle("width","100%");
		
		btn.onClick(this::onSampleButtonPush);

	}

	private void onSampleButtonPush(ButtonPushEvent ev) {
		String text = ed_firstname.getText()+" "+ed_lastname.getText()+ " ("+cb_gender.getText()+")";
		
		App.msgbox(text,0,"Hello World");
	}
	
	
}
