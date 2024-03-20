# Webforj
<p>

[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=webforj&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=webforj)
[![Maven Publish Status](https://github.com/webforj/webforj/actions/workflows/maven-publish.yml/badge.svg)](https://github.com/webforj/webforj/actions/workflows/maven-publish.yml)
[![ReviewDog Status](https://github.com/DwcJava/engine/actions/workflows/reviewdog.yml/badge.svg)](https://github.com/DwcJava/engine/actions/workflows/reviewdog.yml)

</p>

A robust and flexible framework that can help you deliver a modern and engaging web user interface with ease. In Java.

- **Event Handling**: Handle user interactions and events with ease using the webforj's event system. Respond to user actions and update the UI accordingly.

- **Component-Based**: Create reusable and composable components to build complex UIs. Components encapsulate their own state and logic, providing a modular and maintainable structure for your application.

- **Reliable**: Our team in the United States and Europe have been building tools and technologies to help our clients fulfil their needs for over 35 years. We have consistently produced reliable and innovative technologies to ensure our clients have every tool in their kit needed to tackle modern, ever-changing needs.

## Documentation

The webforj's documentation site can be found [at this link](https://documentation.webforj.com/) which contains guides, API references, and examples to help you get started with the webforj.

The following documentation sections may be useful for those beginning their usage of the DWCJ:

- [Webforj Installation](https://documentation.webforj.com/docs/installation/local_install)
- [Configuring your application](https://documentation.webforj.com/docs/getting_started/configuration)
- [Creating an application](https://documentation.webforj.com/docs/getting_started/creating_an_application)
- [Component Overviews](https://documentation.webforj.com/docs/components/home)

## Examples

The Webforj's [HelloWorld repository](https://github.com/webforj/webforj-hello-world) contains a sample program which can be run in GitHub codespaces, Docker, or locally and demonstrates the basics for creating your first DWCJ program. Here is the class created in the sample:

```java
@InlineStyleSheet(/* css */"""
  .mainFrame {
    display: inline-grid;
    gap: 20px;
    margin: 20px;
    padding: 20px;
    border: 1px dotted;
  }
""")
@AppTitle("Webforj Hello World")
public class HelloWorldApp extends App {
  
  Paragraph hello = new Paragraph("Hello World!");
  Button btn = new Button("Say Hello");

  @Override
  public void run() throws WebforjException {

    Frame mainFrame = new Frame();
    mainFrame.addClassName("mainFrame");

    btn.setTheme(ButtonTheme.SUCCESS)
        .setExpanse(Expanse.XLARGE)
        .addClickListener(e -> msgbox("Hello World!"));

    mainFrame.add(hello, btn);
  }
}
```

The above program creates some static text and a button which displays a message box when pushed. 

## Contributing

Contributions to the Webforj project are welcome! If you would like to contribute, please follow the guidelines outlined in the [CONTRIBUTING.md](https://github.com/webforj/webforj/blob/main/CONTRIBUTING.md) file.

## License

The Webforj Engine is licensed under the [MIT License](https://github.com/webforj/webforj/blob/main/LICENSE).
