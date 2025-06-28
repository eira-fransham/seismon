pub fn get_pvs(
    visdata: &[u8],
    vis_offset: usize,
    leaf_count: usize,
) -> impl Iterator<Item = usize> + use<'_> {
    gen move {
        let mut visleaf = 1;
        let mut it = visdata[vis_offset..].iter();

        while visleaf < leaf_count {
            let byte = it.next().unwrap();
            match *byte {
                // a zero byte signals the start of an RLE sequence
                0 => visleaf += 8 * *it.next().unwrap() as usize,

                bits => {
                    for shift in 0..8 {
                        if bits & 1 << shift != 0 {
                            yield visleaf;
                        }

                        visleaf += 1;
                    }
                }
            }
        }
    }
}
