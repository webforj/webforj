// package org.dwcj.extendeddemos.extcomponents;

// import org.dwcj.App;
// import org.dwcj.controls.Label;
// import org.dwcj.events.RatingValueChangedEvent;
// import org.dwcj.exceptions.DwcAppInitializeException;
// import org.dwcj.panels.AppPanel;
// import org.dwcj.shoelacecontrols.Rating;

// public class ShoelaceRating extends App {

//     private Label ratingText;


//     @Override
//     public void run() throws DwcAppInitializeException {

//         AppPanel p = new AppPanel();
//         p.setStyle("padding","20px");

//         Label headline = new Label("<html><h2>Shoelace Rating Demo</h2>");
//         p.add(headline);

//         p.add(new Label("Rating:"));

//         //the rating control from shoelace

//         Rating ratingctrl = new Rating();
//         p.add(ratingctrl);
//         ratingctrl.setStyle("padding-bottom","20px");

//         ratingctrl.onValueChanged(this::onRatingChanged);

//         p.add(new Label(""));
//         ratingText = new Label("");

//         p.add(ratingText);

//     }
//     private void onRatingChanged(RatingValueChangedEvent ratingValueChangedEvent) {

//         Double v = ratingValueChangedEvent.getValue();
//         String txt = "Poor";
//         if (v > 0.0) txt = "Naja";
//         if (v > 1.0) txt = "Hmm";
//         if (v > 2.0) txt = "Just about";
//         if (v > 3.0) txt = "Acceptable";
//         if (v > 4.0) txt = "Supergeil";

//         ratingText.setText("You selected: "+txt);
//     }

// }
