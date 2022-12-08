package org.dwcj.extendeddemos;

import org.dwcj.App;
import org.dwcj.controls.button.Button;
import org.dwcj.controls.button.events.*;
import org.dwcj.controls.label.Label;
import org.dwcj.controls.panels.AppPanel;
import org.dwcj.controls.textarea.TextArea;
import org.dwcj.controls.textbox.TextBox;
import org.dwcj.controls.treeview.TreeView;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.interfaces.IDwcEvent;


public class TreeDemo extends App {

    @Override
    public void run() throws DwcAppInitializeException {

        new TreeDemoApp();
    }

    public class TreeDemoApp extends AppPanel {

        private final TreeView tree;

        private final TextArea multilineEdit;

        private final TextBox nodeTextBox;



        public TreeDemoApp() throws DwcAppInitializeException {

            super();

            //setting some styles of the app panel itself
            setStyle("display", "inline-grid");
            setStyle("grid-template-columns", "1fr 1fr 1fr 1fr 1fr");
            setStyle("gap", "20px");
            setStyle("left", "20px");
            setStyle("top", "20px");
            setStyle("border", "1px dotted");
            setStyle("padding", "10px");

            //collapse all nodes button
            Button collapseButton = new Button("Collapse All");
            add(collapseButton);
            collapseButton.onClick(this::collapseAll);

            //expand all nodes button
            Button expandButton = new Button("Expand All");
            add(expandButton);
            expandButton.onClick(this::expandAll);

            //set node text button
            Button setTextButton = new Button("Set Node Text");
            add(setTextButton);
            setTextButton.onClick(this::setNodeText);

            //set node text: text input field
            Label editLabel = new Label("New Node Text:");
            add(editLabel);
            nodeTextBox = new TextBox();
            add(nodeTextBox);


            tree = new TreeView();
            add(tree);

            tree.setRoot(0,"root");
            tree.addExpandableNode(1,0,"child1");
            tree.addNode(2,0,"child2");
            tree.addNode(3,0,"child3");
            tree.insertNode(6,1,"child6",0);
            tree.insertNode(4,1,"child4",0);
            tree.insertExpandableNode(5,1,"child5",1);

            //log tree events
            tree.onGainedFocus(this::logEvent);
            tree.onLostFocus(this::logEvent);
            tree.onSelect(this::logEvent);
            tree.onDeselect(this::logEvent);
            tree.onExpand(this::logEvent);
            tree.onCollapse(this::logEvent);
            tree.onEditStopped(this::logEvent);
            tree.onDoubleClick(this::logEvent);

            //log buttton events
            collapseButton.onClick(this::logEvent);
            expandButton.onClick(this::logEvent);
            setTextButton.onClick(this::logEvent);


            multilineEdit = new TextArea();
            add(multilineEdit);
            multilineEdit.setVerticalScrollable(true);
            multilineEdit.setHorizontalScrollable(true);
            multilineEdit.setStyle("width","400px");
            multilineEdit.setStyle("height","500px");
        }

        private void collapseAll(ButtonPushEvent ev) { //NOSONAR
            tree.collapseTreeFromNode(tree.getRoot());
            multilineEdit.addParagraph(-1, (multilineEdit.getNumberOfParagraphs() + 1) + ". " + "Method: collapseTreeFromNode(tree.getRoot())");
        }

        private void expandAll(ButtonPushEvent ev) { //NOSONAR
            tree.expandTreeFromNode(tree.getRoot());
            multilineEdit.addParagraph(-1, (multilineEdit.getNumberOfParagraphs() + 1) + ". " + "Method: expandTreeFromNode(tree.getRoot())");
        }

        /**
         * Sets the currently selected node's text to the "New Node Text" input
         *
         * @param ev the button push event that is triggered by clicking on "Set Node Text"
         */
        private void setNodeText(ButtonPushEvent ev) { //NOSONAR
            tree.setNodeText(tree.getSelectedNode(), nodeTextBox.getText());
            multilineEdit.addParagraph(-1, (multilineEdit.getNumberOfParagraphs() + 1) + ". " + "Method: setNodeText(" + tree.getSelectedNode() + ", " + nodeTextBox.getText() + ")");
        }

        private void logEvent(IDwcEvent event) {
            multilineEdit.addParagraph(-1, (multilineEdit.getNumberOfParagraphs() + 1) + ". " + event.toString());
        }
    }
}


