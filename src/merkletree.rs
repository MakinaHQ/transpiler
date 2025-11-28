use alloy::{
    primitives::{FixedBytes, keccak256},
    sol_types::SolValue,
};

// Merkle tree implementation using
// TODO: make this generic over T: AsRef<[u8]>
#[derive(Debug, Clone)]
pub struct MerkleTree {
    hashed_elements: Vec<FixedBytes<32>>,
}

// Public interface impl
impl MerkleTree {
    pub fn new(elements: Vec<FixedBytes<32>>) -> Self {
        let elements = {
            let mut elements: Vec<FixedBytes<32>> = elements
                .into_iter()
                // Filter empty
                .filter(|e| !e.iter().all(|e| *e == 0))
                .collect();

            // Sort
            elements.sort();

            // Deduplicate
            let el_len = elements.len();

            elements
                .into_iter()
                .fold(Vec::with_capacity(el_len), |mut acc, i| {
                    if !acc.contains(&i) {
                        acc.push(i);
                    }
                    acc
                })
        };

        // Construct hashes
        let el_len = elements.len().next_power_of_two();
        let (capacity, levels) = Self::calculate_levels(&el_len);

        let mut result = vec![FixedBytes::default(); capacity];
        tracing::debug!("Creating a vector with size {capacity:}");

        for level in 1..=levels {
            let elem_count_in_level = el_len / level as usize;
            tracing::trace!(
                "level: {level}| capacity: {capacity}| elem_count_in_level {elem_count_in_level}",
            );
            let inverse_level = levels - level;
            let start_index = 2_usize.pow(inverse_level) - 1;

            let end_index = start_index + elem_count_in_level; // non inclusive
            tracing::trace!(
                "start_index: {start_index}| end_index {end_index}| elem_count_in_level {elem_count_in_level}"
            );

            if level == 1 {
                for (idx, elem) in elements.iter().enumerate() {
                    let hashed = keccak256(elem);
                    tracing::debug!("Setting idx {:} to {:}", start_index + idx, hashed,);
                    result[start_index + idx] = hashed;
                }
            } else {
                for idx in start_index..end_index {
                    let left = (2_usize * idx) + 1;
                    let right = (2_usize * idx) + 2;

                    tracing::trace!("Getting child of {idx}| L: {left}| R: {right}");
                    let left = result[left];
                    let right = result[right];

                    if left < right {
                        result[idx] = keccak256((left, right).abi_encode());
                    } else {
                        result[idx] = keccak256((right, left).abi_encode());
                    }
                }
            }
        }

        let res = Self {
            hashed_elements: result,
        };
        tracing::debug!("Constructed merkle tree {:#?}", &res);
        res
    }

    pub fn get_root(&self) -> &FixedBytes<32> {
        &self.hashed_elements[0]
    }

    pub fn get_proof(&self, el: &FixedBytes<32>) -> Option<Vec<FixedBytes<32>>> {
        let hashed = keccak256(el);
        tracing::debug!("Finding proof for {hashed:}");

        let index = self.hashed_elements.iter().position(|e| e == &hashed);

        match index {
            Some(mut index) => {
                let mut res = vec![];

                while index > 0 {
                    // Skip the root element
                    let sibling = self.get_pair_element(index);

                    if let Some(sibling) = sibling {
                        tracing::trace!("getting pair elem for index {index:}; res {sibling:}");
                        res.push(*sibling);
                    }

                    index = MerkleTree::calculate_parent_idx(index);
                    tracing::trace!("Parent {index:}");
                }
                Some(res)
            }
            None => None,
        }
    }

    fn get_pair_element(&self, idx: usize) -> Option<&FixedBytes<32>> {
        let pair_idx = Self::calculate_sibling_idx(idx);
        tracing::trace!("Pair index for {idx} is {pair_idx}");
        if pair_idx < self.hashed_elements.len() {
            return Some(&self.hashed_elements[pair_idx]);
        }
        None
    }

    fn calculate_sibling_idx(idx: usize) -> usize {
        if idx.is_multiple_of(2) {
            idx - 1
        } else {
            idx + 1
        }
    }

    fn calculate_parent_idx(child_idx: usize) -> usize {
        let child_offset = {
            if child_idx.is_multiple_of(2) {
                // If is right child
                2
            } else {
                // If is left child
                1
            }
        };

        (child_idx - child_offset) / 2
    }

    fn calculate_levels(el_len: &usize) -> (usize, u32) {
        let capacity = (2 * el_len).next_power_of_two() - 1;
        let levels: u32 = ((capacity as f32).log2() + 1.) as u32;
        (capacity, levels)
    }
}
