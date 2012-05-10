/**
 * Copyright 2011-2012 eBusiness Information, Groupe Excilys (www.excilys.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.excilys.ebi.gatling.recorder.configuration;

import static org.apache.commons.lang.StringUtils.EMPTY;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.excilys.ebi.gatling.recorder.ui.enumeration.FilterStrategy;

public class Configuration {

	public static Configuration getConfigurationInstance() {
		return instance;
	}

	public static void initConfigurationFromExistingConfig(Configuration cconfiguration) {
		instance.setPort(cconfiguration.getPort());
		instance.setSslPort(cconfiguration.getSslPort());
		instance.setProxy(cconfiguration.getProxy());
		instance.setFollowRedirect(cconfiguration.isFollowRedirect());
		instance.setSimulationClassName(cconfiguration.getSimulationClassName());
		instance.setSimulationPackage(cconfiguration.getSimulationPackage());
		instance.setFilterStrategy(cconfiguration.getFilterStrategy());
		instance.setPatterns(cconfiguration.getPatterns());
		instance.setOutputFolder(cconfiguration.getOutputFolder());
		instance.setSaveConfiguration(true);
		instance.setEncoding(cconfiguration.getEncoding());
		instance.setRequestBodiesFolder(cconfiguration.getRequestBodiesFolder());
	}

	public static void initConfigurationFromCommandLineOptions(CommandLineOptions cli) {
		instance.setPort(cli.getLocalPort());
		instance.setSslPort(cli.getLocalPortSsl());
		instance.getProxy().setHost(cli.getProxyHost());
		instance.getProxy().setPort(cli.getProxyPort());
		instance.getProxy().setSslPort(cli.getProxyPortSsl());
		if (cli.getOutputFolder() != null)
			instance.setOutputFolder(cli.getOutputFolder());
		if (cli.getSimulationClassName() != null)
			instance.setSimulationClassName(cli.getSimulationClassName());
		if (cli.getSimulationPackage() != null)
			instance.setSimulationPackage(cli.getSimulationPackage());
		if (cli.getRequestBodiesFolder() != null)
			instance.setRequestBodiesFolder(cli.getRequestBodiesFolder());
		if (cli.getEncoding() != null)
			instance.setEncoding(cli.getEncoding());
	}

	private static final Configuration instance = new Configuration();

	private int port = 8000;
	private int sslPort = 8001;
	private ProxyConfig proxy = new ProxyConfig();
	private boolean followRedirect;
	private FilterStrategy filterStrategy = FilterStrategy.NONE;
	private List<Pattern> patterns = new ArrayList<Pattern>();
	private String outputFolder = System.getenv("GATLING_HOME") + File.separator + "user-files" + File.separator + "simulations";
	private boolean saveConfiguration;
	private String encoding = "UTF-8";
	private String requestBodiesFolder = System.getenv("GATLING_HOME") + File.separator + "user-files" + File.separator + "request-bodies";
	private String simulationPackage = EMPTY;
	private String simulationClassName = "Simulation";

	private Configuration() {

	}

	@Override
	public String toString() {
		return new StringBuilder().append("Configuration [port=").append(port).append(", sslPort=").append(sslPort).append(", proxy=").append(proxy).append(", filterStrategy=")
		        .append(filterStrategy).append(", patterns=").append(patterns).append(", outputFolder=").append(outputFolder).append(", saveConfiguration=")
		        .append(saveConfiguration).append("]").toString();
	}

	public int getPort() {
		return port;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public ProxyConfig getProxy() {
		return proxy;
	}

	public void setProxy(ProxyConfig proxy) {
		this.proxy = proxy;
	}

	public FilterStrategy getFilterStrategy() {
		return filterStrategy;
	}

	public void setFilterStrategy(FilterStrategy filterStrategy) {
		this.filterStrategy = filterStrategy;
	}

	public List<Pattern> getPatterns() {
		return patterns;
	}

	public void setPatterns(List<Pattern> patterns) {
		this.patterns = patterns;
	}

	public String getOutputFolder() {
		return outputFolder;
	}

	public void setOutputFolder(String outputFolder) {
		this.outputFolder = outputFolder;
	}

	public int getSslPort() {
		return sslPort;
	}

	public void setSslPort(int sslPort) {
		this.sslPort = sslPort;
	}

	public boolean isSaveConfiguration() {
		return saveConfiguration;
	}

	public void setSaveConfiguration(boolean saveConfiguration) {
		this.saveConfiguration = saveConfiguration;
	}

	public String getSimulationPackage() {
		return simulationPackage;
	}

	public void setSimulationPackage(String simulationPackage) {
		this.simulationPackage = simulationPackage;
	}

	public String getSimulationClassName() {
		return simulationClassName;
	}

	public void setSimulationClassName(String simulationClassName) {
		this.simulationClassName = simulationClassName;
	}

	public String getRequestBodiesFolder() {
		return requestBodiesFolder;
	}

	public void setRequestBodiesFolder(String requestBodiesFolder) {
		this.requestBodiesFolder = requestBodiesFolder;
	}

	public String getEncoding() {
		return encoding;
	}

	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}

	public boolean isFollowRedirect() {
		return followRedirect;
	}

	public void setFollowRedirect(boolean followRedirect) {
		this.followRedirect = followRedirect;
	}
}
