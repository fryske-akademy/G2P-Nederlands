# G2P-Nederlands
Software for converting Dutch text to phonetic IPA transcriptions.

Follow the instructions below to build the Docker image and launch the container.

### 1. Clone the Repo

```
git clone https://github.com/fryske-akademy/G2P-Nederlands.git
cd G2P-Nederlands
```

### 2. Build the Docker Image

```
docker build -t g2p-nederlands .
```

### 3. Run the Container

```
docker run -p 3838:3838 g2p-nederlands
```

### 4. View in Browser

Open:
http://localhost:3838
