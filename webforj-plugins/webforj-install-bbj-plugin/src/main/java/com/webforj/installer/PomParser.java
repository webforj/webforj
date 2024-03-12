package com.webforj.installer;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 * This class is responsible for parsing the POM so the installer can access the configuration
 * properties in the webforj-install-maven-plugin.
 */
public class PomParser {
  Logger log = LogManager.getLogger(PomParser.class);

  private final File pomFile;

  /**
   * XPath expression to get the plugin configuration children.
   */
  String expression = """
      //plugin[groupId="com.webforj" and artifactId="webforj-install-maven-plugin"]/configuration/*
      """;

  /**
   * Constructor.
   *
   * @param pomFile the actual File object containing the pom.
   */
  public PomParser(File pomFile) {
    log.info("receiving pomFile {}", pomFile);
    this.pomFile = pomFile;
  }

  /**
   * Constructor.
   *
   * @param pomfile - the path to find the pom.xml
   */
  public PomParser(String pomfile) {
    this(new File(pomfile));
  }

  /**
   * Parse the pom file, discovering webforj-install-maven-plugin declaration and scraping the
   * configuration children.
   *
   * @return a map whose keys are the name of the config child and value is the text content.
   * @throws RuntimeException wrapping any other exception that happens.
   */
  public Map<String, String> parse() throws IOException {
    log.info("parsing pom file");
    try {
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      dbFactory.setFeature("http://xml.org/sax/features/external-general-entities", false);
      dbFactory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
      dbFactory.setIgnoringComments(true);
      log.info("Created document to read xml");
      DocumentBuilder docBuilder = dbFactory.newDocumentBuilder();
      docBuilder.setErrorHandler(null);
      Document doc = docBuilder.parse(pomFile);
      doc.getDocumentElement().normalize();
      XPath xPath = XPathFactory.newInstance().newXPath();
      NodeList nodeList =
          (NodeList) xPath.compile(expression).evaluate(doc, XPathConstants.NODESET);
      return IntStream.range(0, nodeList.getLength()).mapToObj(nodeList::item)
          .map(Element.class::cast)
          .collect(Collectors.toMap(Element::getNodeName, Element::getTextContent));
    } catch (Exception e) {
      log.info("Exception processing pom!", e);
      throw new IOException(e);
    }
  }

}
