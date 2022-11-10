// SPDX-License-Identifier: MIT LICENSE

import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Enumerable.sol";

pragma solidity ^0.8.13;

contract NFTCollection is ERC721Enumerable, Ownable {
    using Strings for uint256;
    string public baseURI;
    string public baseExtension = ".json";
    uint256 public maxSupply = 10;
    uint256 public maxMintAmount = 10;
    bool public paused = false;

    constructor(string memory name_, string memory symbol_)
        ERC721(name_, symbol_)
    {}

    function _baseURI() internal view virtual override returns (string memory) {
        return "ipfs://QmYB5uWZqfunBq7yWnamTqoXWBAHiQoirNLmuxMzDThHhi/";
    }

    function mint(address to_, uint256 mintAmount_) public payable onlyOwner {
        uint256 supply = totalSupply();
        require(!paused);
        require(mintAmount_ > 0);
        require(mintAmount_ <= maxMintAmount);
        require(supply + mintAmount_ <= maxSupply);

        for (uint256 i = 1; i <= mintAmount_; i++) {
            _safeMint(to_, supply + i);
        }
    }

    function walletOfOwner(address owner_)
        public
        view
        returns (uint256[] memory)
    {
        uint256 ownerTokenCount = balanceOf(owner_);
        uint256[] memory tokenIds = new uint256[](ownerTokenCount);
        for (uint256 i; i < ownerTokenCount; i++) {
            tokenIds[i] = tokenOfOwnerByIndex(owner_, i);
        }
        return tokenIds;
    }

    function tokenURI(uint256 tokenID)
        public
        view
        virtual
        override
        returns (string memory)
    {
        require(
            _exists(tokenID),
            "ERC721Metadata: URI query for nonexistent token"
        );

        string memory currentBaseURI = _baseURI();
        return
            bytes(currentBaseURI).length > 0
                ? string(
                    abi.encodePacked(
                        currentBaseURI,
                        tokenID.toString(),
                        baseExtension
                    )
                )
                : "";
    }

    // only owner

    function setmaxMintAmount(uint256 newmaxMintAmount_) public onlyOwner {
        maxMintAmount = newmaxMintAmount_;
    }

    function setBaseURI(string memory newBaseURI_) public onlyOwner {
        baseURI = newBaseURI_;
    }

    function setBaseExtension(string memory newBaseExtension_)
        public
        onlyOwner
    {
        baseExtension = newBaseExtension_;
    }

    function pause(bool state_) public onlyOwner {
        paused = state_;
    }

    function withdraw() public payable onlyOwner {
        require(payable(msg.sender).send(address(this).balance));
    }
}
