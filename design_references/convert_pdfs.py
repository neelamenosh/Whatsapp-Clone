import fitz  # PyMuPDF
import os

input_dir = "/Users/user/Whatsapp-Clone/design_references"
output_dir = "/Users/user/Whatsapp-Clone/design_references/images"
os.makedirs(output_dir, exist_ok=True)

for filename in os.listdir(input_dir):
    if filename.endswith(".pdf"):
        pdf_path = os.path.join(input_dir, filename)
        doc = fitz.open(pdf_path)
        base_name = os.path.splitext(filename)[0]
        
        for page_num in range(len(doc)):
            page = doc.load_page(page_num)
            # Use higher resolution for better quality
            mat = fitz.Matrix(2, 2)  # 2x zoom
            pix = page.get_pixmap(matrix=mat)
            output_path = os.path.join(output_dir, f"{base_name}_page_{page_num + 1}.png")
            pix.save(output_path)
            print(f"Saved: {output_path}")
        
        doc.close()

print("\nDone! All PDFs converted to images.")
