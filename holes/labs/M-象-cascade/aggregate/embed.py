import json, numpy as np
from sentence_transformers import SentenceTransformer
it=json.load(open('items.json'))
m=SentenceTransformer('sentence-transformers/all-MiniLM-L6-v2')
v=m.encode([i['text'] for i in it],batch_size=64,normalize_embeddings=True,show_progress_bar=False)
np.save('vectors.npy',v.astype('float32')); print(v.shape)
